# ------------------------------------------------------------------------------
# SENTIMENT CLASSIFICATION PIPELINE
# ------------------------------------------------------------------------------

# IN: data filtered for subjective tweets
# OUT: data with sentiment labels

# MAKE CLASSIFICATION TASK -----------------------------------------------------

# Make task

load_rdata_files(tweets_subjective, folder = "2_code/1_data/2_tmp_data")

data <- tweets_subjective[
  label != "none"]

data$label <- as.factor(data$label)

data.table::setorder(data, doc_id)

task <- mlr3::TaskClassif$new(
  "twitter_ttsa", 
  backend = data,
  target = "label")

# Create stratified train-test split

indices_positive <- data.table::copy(task$data())[
  , row_indices := .I
  ][label == "positive", row_indices]
indices_negative <- setdiff(seq_len(task$nrow), indices_positive)

set.seed(123L)
train_set = sort(union(
  sample(indices_positive, 0.7 * length(indices_positive)),
  sample(indices_negative, 0.7 * length(indices_negative))))
test_set = setdiff(seq_len(task$nrow), train_set)

stopifnot(abs(
  table(task$data()[train_set]$label)["positive"] / length(train_set) -
    table(task$data()[test_set]$label)["positive"] / length(test_set)) < 5e-2)

# CREATE PREPROCESSING PIPELINE ------------------------------------------------

# Define topic modeling type (parameters need to be set within if clause)

topic_type <- c("stm", "keyword")[1L]

if (topic_type == "stm") {
  
  # Define topic modeling pipeop
  
  po_topic_modeling <- PipeOpExtractTopicsSTM$new()
  
  po_topic_modeling$param_set$values <- list(
    docid_field = "doc_id",
    text_field = "text",
    doc_grouping_var = c("twitter_username", "twitter_year", "twitter_month"),
    prevalence_vars_cat = list("meta_party", "meta_bundesland"),
    prevalence_vars_smooth = list("meta_unemployment_rate"),
    max.em.its = 100L,
    stopwords = make_stopwords(),
    init.type = "LDA")
  
} else if (topic_type == "keyword") {
  
  keywords <- list(
    corona = c("corona", "pandemie", "virus", "krise"),
    klima = c("klima", "umwelt"))
  
  po_stratify <- PipeOpStratifyKeywords$new()
  
  po_stratify$param_set$values <- list(
    docid_field = "doc_id",
    text_field = "text",
    stopwords = make_stopwords(),
    keywords = keywords)

  po_set_strata <- mlr3pipelines::PipeOpColRoles$new()
  
  strata <- as.list(rep("stratum", length(keywords)))
  names(strata) <- sprintf("stratum_%s", seq_along(keywords))

  po_set_strata$param_set$values$new_role <- strata

  po_topic_modeling <- PipeOpExtractTopicsKeyword$new()

  po_topic_modeling$param_set$values <- list(
    docid_field = "doc_id",
    text_field = "text",
    stopwords = make_stopwords(),
    keywords = keywords,
    n_byterms = 10L)
  
}

# graph_preproc <- mlr3pipelines::Graph$new()$add_pipeop(po_sk)
# res_preproc <- graph_preproc$train(task)[[1]]
# res_preproc$data()
# table(res_preproc$data()$is_match_any)

# Define selector pipeop for features to be piped into embedding extraction

po_sel_embeddings <- mlr3pipelines::PipeOpSelect$new(id = "select_embedding")

po_sel_embeddings$param_set$values$selector <- 
  mlr3pipelines::selector_name(c("topic_label", "text"))

po_sel_embeddings_inv <- mlr3pipelines::PipeOpSelect$new(id = "select_rest")

po_sel_embeddings_inv$param_set$values$selector <- 
  mlr3pipelines::selector_invert(
    mlr3pipelines::selector_name(c("topic_label", "text")))

# Define glove embedding pipeop

po_embeddings <- PipeOpMakeGloveEmbeddings$new()
po_embeddings$param_set$values$stopwords <- make_stopwords()

# Define trivial pipeop for features not piped into embedding extraction

po_nop <- mlr3pipelines::PipeOpNOP$new(id = "pipe_along")

# Define selector pipeop for features to be piped into classification
# TODO tailor to learners (e.g., cart could handle factors)

po_sel_sentiment_analysis <- 
  mlr3pipelines::PipeOpSelect$new(id = "select_sentiment_analysis")

po_sel_sentiment_analysis$param_set$values$selector <- 
  mlr3pipelines::selector_union(
    mlr3pipelines::selector_name("doc_id"),
    mlr3pipelines::selector_grep("feat.*"))

# Define pipeop to set col role for doc_id to naming column

po_set_colroles <- mlr3pipelines::PipeOpColRoles$new(id = "set_colroles")
po_set_colroles$param_set$values$new_role <- list(doc_id = "name")

# Create graph from preprocessing steps

if (topic_type == "stm") {
  
  graph_preproc <- mlr3pipelines::Graph$new()$add_pipeop(po_topic_modeling)
  
} else if (topic_type == "keyword") {
  
  graph_preproc <- mlr3pipelines::Graph$new()$add_pipeop(po_stratify) %>>% 
    po_set_strata %>>% 
    po_topic_modeling
  
}

# res_tm_train <- graph_preproc$train(task$clone()$filter(train_set))[[1]]
# res_tm_test <- graph_preproc$predict(task$clone()$filter(test_set))[[1]]

graph_preproc <- graph_preproc %>>%
  gunion(list(
    po_sel_embeddings %>>% 
      po_embeddings,
    po_sel_embeddings_inv %>>% 
      po_nop)
    ) %>>%
  po("featureunion", id = "unite_features") %>>%
  po_sel_sentiment_analysis %>>%
  po_set_colroles

plot(graph_preproc, html = FALSE)

# res_tm_train <- graph_preproc$train(task$clone()$filter(train_set))[[1]]
# res_tm_train$col_roles

# CREATE GRAPH LEARNERS --------------------------------------------------------

# Define pipeops for different classifiers

po_learners <- list(
  forest = mlr3pipelines::PipeOpLearner$new(
    learner = lrn("classif.ranger"), 
    id = "classify_forest"),
  svm = mlr3pipelines::PipeOpLearner$new(
    learner = lrn("classif.svm", type = "C-classification"), 
    id = "classify_svm"),
  lasso = mlr3pipelines::PipeOpLearner$new(
    learner = lrn("classif.cv_glmnet"), 
    id = "classify_lasso"))

# Create full graphs

graphs_full <- lapply(po_learners, function(i) graph_preproc %>>% i)
plot(graphs_full$forest, html = FALSE)

# Create graph_learners

graph_learners <- lapply(
  seq_along(graphs_full),
  function(i) GraphLearner$new(graphs_full[[i]], id = names(po_learners)[i]))
names(graph_learners) <- names(po_learners)

# graph_learners[[1]]$train(task, row_ids = train_set)
# res <- graph_learners[[1]]$predict(task, row_ids = test_set)
# res$confusion

# BENCHMARK LEARNERS -----------------------------------------------------------

# Define tuning parameters

resampling_inner <- mlr3::rsmp("cv", folds = 3L)
measure_inner <- mlr3::msr("classif.acc")
terminator <- mlr3tuning::trm("evals", n_evals = 3L)
tuner <- mlr3tuning::tnr("grid_search", resolution = 10L)

# Define search spaces

param_set_base <- paradox::ParamSet$new(list(
  paradox::ParamInt$new("extract_topics_stm.K"),
  paradox::ParamInt$new("make_glove_embeddings.dimension")))
param_set_base$values <- list(
  extract_topics_stm.K = paradox::to_tune(5L, 10L),
  make_glove_embeddings.dimension = paradox::to_tune(5L, 10L))

param_set_forest <- param_set_base$clone()
param_set_forest$add(paradox::ParamInt$new("classify_forest.mtry"))
param_set_forest$values$classify_forest.mtry <- paradox::to_tune(7L, 30L)

param_set_svm <- param_set_base$clone()
param_set_svm$add(paradox::ParamSet$new(list(
  paradox::ParamFct$new(
    "classify_svm.kernel",
    levels = c("polynomial", "radial")),
  paradox::ParamDbl$new("classify_svm.gamma"))))
param_set_svm$values$classify_svm.kernel <- paradox::to_tune()
param_set_svm$values$classify_svm.gamma <- paradox::to_tune(1e-3, 1e-1)

param_set_lasso <- param_set_base$clone()
param_set_lasso$add(paradox::ParamDbl$new("classify_lasso.alpha"))
param_set_lasso$values$classify_lasso.alpha <- paradox::to_tune(1e-2, 1L)

param_sets <- list(param_set_forest, param_set_svm, param_set_lasso)

# Define tuning learners

auto_tuners <- lapply(
  seq_along(graph_learners), 
  function(i) {
    mlr3tuning::AutoTuner$new(
      learner = graph_learners[[i]],
      resampling = resampling_inner,
      measure = measure_inner,
      search_space = param_sets[[i]]$search_space(),
      terminator = terminator,
      tuner = tuner)})

# auto_tuners[[1]]$train(task, row_ids = train_set)
# res_cart <- auto_tuners[[1]]$predict(task, row_ids = test_set)
# res_cart$confusion

# auto_tuners[[2]]$train(task, row_ids = train_set)
# res_glmnet <- auto_tuners[[2]]$predict(task, row_ids = test_set)
# res_glmnet$confusion

# Define benchmarking parameters

resampling_outer <- mlr3::rsmp("cv", folds = 3L)

# Define benchmarking design

bmr_design <- mlr3::benchmark_grid(
  task$clone()$filter(train_set), 
  auto_tuners, 
  resampling_outer)

# Run benchmark 

future::plan("multiprocess")
set.seed(123L)
bmr <- mlr3::benchmark(bmr_design)

benchmark_results <- data.table::as.data.table(bmr)

save_rdata_files(
  benchmark_results, 
  folder = "2_code/1_data/2_tmp_data",
  tmp = FALSE)

perf_bmr <- bmr$score(msr("classif.acc", id = "accuracy"))
winner <- which.max(perf_bmr$accuracy)

# EVALUATE PERFORMANCE OF SELECTED LEARNER -------------------------------------

winning_learner <- benchmark_results$learner[[winner]]$learner
winning_learner$train(task, row_ids = train_set)
winning_learner$param_set

perf <- winning_learner$predict(task, row_ids = test_set)

perf$confusion
perf$score(msr("classif.acc"))
