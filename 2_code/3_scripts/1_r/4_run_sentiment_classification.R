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

# labels_fake <- sample(
#   c("positive", "negative"),
#   size = nrow(tweets_subjective[label == "none"]),
#   replace = TRUE)
# 
# data_fake <- tweets_subjective[
#   label == "none"
#   ][, label := labels_fake]
# 
# data_fake <- data_fake[sample(seq_len(nrow(data_fake)), size = 1000L)]
# 
# data <- rbind(data, data_fake)

data$label <- as.factor(data$label)

# TODO make complete cases better (i.e., only taking into account relevant vars)

task <- mlr3::TaskClassif$new(
  "twitter_ttsa", 
  backend = data[complete.cases(data)],
  target = "label")

# Create train-test split

set.seed(123L)
train_set = sample(task$nrow, 0.7 * task$nrow)
test_set = setdiff(seq_len(task$nrow), train_set)

# CREATE PREPROCESSING PIPELINE ------------------------------------------------

topic_type <- "stm"

# topic_type <- "keyword"

keywords <- list(
  corona = c("corona", "pandemie", "virus", "krise"),
  klima = c("klima", "umwelt"))

if (topic_type == "stm") {
  
  # Define topic modeling pipeop
  
  prevalence_formula <- make_prevalence_formula(
    data = task$data(),
    categorical_vars = list(
      "meta_party", 
      "meta_bundesland"),
    smooth_vars = list(
      "meta_unemployment_rate"))
  
  po_tm <- PipeOpExtractTopicsSTM$new()
  
  po_tm$param_set$values <- list(
    docid_field = "doc_id",
    text_field = "text",
    doc_grouping_var = c("twitter_username", "twitter_year", "twitter_month"),
    prevalence = prevalence_formula,
    max.em.its = 5L,
    stopwords = make_stopwords(),
    K = 10L,
    init.type = "LDA")
  
} else if (topic_type == "keyword") {
  
  po_sk <- PipeOpStratifyKeywords$new()
  
  po_sk$param_set$values <- list(
    docid_field = "doc_id",
    text_field = "text",
    stopwords = make_stopwords(),
    keywords = keywords)

  po_cr <- mlr3pipelines::PipeOpColRoles$new()
  
  strati <- as.list(rep("stratum", length(keywords)))
  names(strati) <- sprintf("stratum_%s", seq_along(keywords))

  po_cr$param_set$values$new_role <- strati

  po_tm <- PipeOpExtractTopicsKeyword$new()

  po_tm$param_set$values <- list(
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

po_sel_ge <- mlr3pipelines::PipeOpSelect$new(id = "select_embedding")

po_sel_ge$param_set$values$selector <- 
  mlr3pipelines::selector_name(c("topic_label", "text"))

po_sel_ge_inv <- mlr3pipelines::PipeOpSelect$new(id = "select_rest")

po_sel_ge_inv$param_set$values$selector <- mlr3pipelines::selector_invert(
  mlr3pipelines::selector_name(c("topic_label", "text")))

# Define glove embedding pipeop

po_ge <- PipeOpMakeGloveEmbeddings$new()
po_ge$param_set$values$dimension <- 3L
po_ge$param_set$values$stopwords <- make_stopwords()

# Define trivial pipeop for features not piped into embedding extraction

po_nop <- mlr3pipelines::PipeOpNOP$new()

# Define selector pipeop for features to be piped into classification
# TODO tailor to learners (e.g., cart could handle factors)

po_sel_cl <- mlr3pipelines::PipeOpSelect$new(id = "select_classif")

po_sel_cl$param_set$values$selector <- 
  mlr3pipelines::selector_invert(
    mlr3pipelines::selector_union(
      mlr3pipelines::selector_type(
        c("character", "POSIXct", "logical", "factor")),
      mlr3pipelines::selector_missing()))

# Create graph from preprocessing steps

if (topic_type == "stm") {
  
  graph_preproc <- mlr3pipelines::Graph$new()$add_pipeop(po_tm)
  
} else if (topic_type == "keyword") {
  
  graph_preproc <- mlr3pipelines::Graph$new()$add_pipeop(po_sk) %>>% 
    po_cr %>>% 
    po_tm
  
}

graph_preproc <- graph_preproc %>>%
  gunion(list(
    po_sel_ge %>>% 
      po_ge,
    po_sel_ge_inv %>>% 
      po_nop)
    ) %>>%
  po("featureunion") %>>%
  po_sel_cl

# plot(graph_preproc, html = FALSE)

# graph_preproc <- mlr3pipelines::Graph$new()$add_pipeop(po_tm)
# res_preproc_train <- graph_preproc$train(task$clone()$filter(train_set))[[1]]
# res_preproc_test <- graph_preproc$predict(task$clone()$filter(test_set))[[1]]

# CREATE GRAPH LEARNERS --------------------------------------------------------

# Define pipeops for different classifiers

po_learners <- list(
  cart = PipeOpLearner$new(learner = lrn("classif.rpart")),
  glmnet = PipeOpLearner$new(
    learner = lrn("classif.cv_glmnet")))

# Create full graphs

graphs_full <- lapply(po_learners, function(i) graph_preproc %>>% i)

# plot(graphs_full$cart, html = F)

# Create graph_learners

graph_learners <- lapply(
  seq_along(graphs_full),
  function(i) GraphLearner$new(graphs_full[[i]], id = names(po_learners)[i]))

graph_learners[[1]]$train(task, row_ids = train_set)
res <- graph_learners[[1]]$predict(task, row_ids = test_set)
res$confusion

# BENCHMARK LEARNERS -----------------------------------------------------------

# Define tuning parameters

resampling_inner <- mlr3::rsmp("cv", folds = 5L)
measure_inner <- mlr3::msr("classif.ce")
terminator <- mlr3tuning::trm("evals", n_evals = 5L)
tuner <- mlr3tuning::tnr("grid_search", resolution = 10L)

# Attention: svm crashes (error length(response) < length(prediction data) - 
# sth seems to be dropped along the way)

search_spaces <- list(
  cart = paradox::ParamSet$new(
    params = list(
      paradox::ParamInt$new(
        "classif.rpart.minsplit", 
        lower = 20L, 
        upper = 25L))),
  glmnet = paradox::ParamSet$new(
    params = list(
      paradox::ParamDbl$new(
        "classif.cv_glmnet.alpha", 
        lower = 0.9, 
        upper = 1L))))

# Define tuning learners

auto_tuners <- lapply(
  seq_along(graph_learners), 
  function(i) {
    mlr3tuning::AutoTuner$new(
      learner = graph_learners[[i]],
      resampling = resampling_inner,
      measure = measure_inner,
      search_space = search_spaces[[i]],
      terminator = terminator,
      tuner = tuner)})

# auto_tuners[[1]]$train(task, row_ids = train_set)
# res_cart <- auto_tuners[[1]]$predict(task, row_ids = test_set)
# res_cart$confusion
# 
# auto_tuners[[2]]$train(task, row_ids = train_set)
# res_glmnet <- auto_tuners[[2]]$predict(task, row_ids = test_set)
# res_glmnet$confusion

# Define benchmarking parameters

resampling_outer <- mlr3::rsmp("cv", folds = 5L)

measures_outer <- list(
  mlr3::msr("classif.ce", id = "mmce_test"))

# Define benchmarking design

bmr_design <- mlr3::benchmark_grid(task, auto_tuners, resampling_outer)

# Run benchmark 

bmr <- mlr3::benchmark(bmr_design)
bmr_res <- bmr$aggregate(measures_outer)

benchmark_results <- data.table::as.data.table(bmr)

save_rdata_files(benchmark_results, folder = "2_code/1_data/2_tmp_data")
