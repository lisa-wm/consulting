# ------------------------------------------------------------------------------
# SENTIMENT CLASSIFICATION PIPELINE
# ------------------------------------------------------------------------------

# IN: corpus object of cleaned tweets and meta data
# OUT: data with sentiment labels

# MAKE CLASSIFICATION TASK -----------------------------------------------------

# Load data

load_rdata_files(tweets_corpus, folder = "2_code/1_data/2_tmp_data")
tweets_sa <- convert_qtda_to_dt(tweets_corpus, key = "doc_id")

cols_to_keep <- c(
  "doc_id",
  "label",
  "text",
  names(tweets_sa)[startsWith(names(tweets_sa), "feat")],
  "meta_party",
  "meta_bundesland",
  "meta_unemployment_rate",
  "twitter_username",
  "twitter_year",
  "twitter_month")

char_cols <- sprintf("feat_%s", letters)
tweets_sa <- tweets_sa[, ..cols_to_keep]
tweets_sa$label <- as.factor(tweets_sa$label)

# Make task

data_labeled <- tweets_sa[label != "none"]
data_unlabeled <- tweets_sa[label == "none"]

stopifnot(nrow(data_labeled) + nrow(data_unlabeled) == nrow(tweets_sa))

invisible(sapply(
  list(data_labeled, data_unlabeled), 
  data.table::setorder, doc_id))

task <- mlr3::TaskClassif$new(
  "sentiment_analysis", 
  backend = data_labeled,
  target = "label")

# Create stratified train-test split (data are only moderately imbalanced)

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

# CREATE PRE-PROCESSING PIPELINES ----------------------------------------------

# Create two different graph learners to benchmark against each other (one with,
# one without upstream topic modeling)

# Create topic modeling pipeop (defined in separate function)

po_tm <- PipeOpExtractTopicsSTM$new()

po_tm$param_set$values <- list(
  K = 3L,
  docid_field = "doc_id",
  text_field = "text",
  doc_grouping_var = c("twitter_username", "twitter_year", "twitter_month"),
  prevalence_vars_cat = list("meta_party", "meta_bundesland"),
  prevalence_vars_smooth = list("meta_unemployment_rate"),
  max.em.its = 5L,
  stopwords = make_stopwords(),
  init.type = "LDA")

ppl_with_tm = Graph$new()$add_pipeop(po_tm)

# foo <- ppl_with_tm$train(task$clone())[[1]]
# head(foo$data())
# 
# foofoo <- ppl_with_tm$predict(task$clone())[[1]]
# head(foofoo$data())

preproc_pipelines <- list(
  ppl_with_tm = Graph$new()$add_pipeop(po_tm), 
  ppl_without_tm = mlr3pipelines::Graph$new()$add_pipeop(
    mlr3pipelines::po("nop", id = "start")))

# Create pipelines

preproc_pipelines <- lapply(
  
  names(preproc_pipelines),
  
  function(i) {
  
    # Create selector pipeop for features to be piped into embedding extraction
    
    po_sel_embeddings <- 
      mlr3pipelines::PipeOpSelect$new(id = "select_embedding")
    po_sel_embeddings_inv <- mlr3pipelines::po("select", id = "select_rest")
    
    po_sel_embeddings$param_set$values$selector <- switch(
      i,
      ppl_with_tm = mlr3pipelines::selector_name(c("topic_label", "text")),
      ppl_without_tm = mlr3pipelines::selector_name(c("text")))
    
    po_sel_embeddings_inv$param_set$values$selector <- switch(
      i,
      ppl_with_tm = mlr3pipelines::selector_invert(
        mlr3pipelines::selector_name(c("topic_label", "text"))),
      ppl_without_tm = mlr3pipelines::selector_invert(
        mlr3pipelines::selector_name(c("text"))))
    
    # Create glove embedding pipeop (defined in separate function)
    
    po_embeddings <- PipeOpMakeGloveEmbeddings$new()
    po_embeddings$param_set$values$stopwords <- make_stopwords()
    po_embeddings$param_set$values$dimension <- 3L
    
    # Create trivial pipeop for features not piped into embedding extraction
    
    po_nop <- mlr3pipelines::po("nop", id = "pipe_along")
    
    # Create selector pipeop for features to be piped into classification
    
    po_sel_sentiment_analysis <- 
      mlr3pipelines::po("select", id = "select_sentiment_analysis")
    
    po_sel_sentiment_analysis$param_set$values$selector <- 
      mlr3pipelines::selector_union(
        mlr3pipelines::selector_name("doc_id"),
        mlr3pipelines::selector_grep("feat.*|embedding.*"))
    
    # Create pipeop to set column role for doc_id to naming column
    
    po_set_colroles <- mlr3pipelines::po("colroles", id = "set_colroles")
    po_set_colroles$param_set$values$new_role <- list(doc_id = "name")
    
    # Create graph from pre-processing steps

    preproc_pipelines[[i]] %>>%
      mlr3pipelines::gunion(list(
        po_sel_embeddings %>>% 
          po_embeddings,
        po_sel_embeddings_inv %>>% 
          po_nop)) %>>%
      mlr3pipelines::po("featureunion", id = "unite_features") %>>%
      po_sel_sentiment_analysis %>>%
      po_set_colroles
      
  }
)

invisible(lapply(preproc_pipelines, plot))

if (FALSE) {
  
  foo <- preproc_pipelines[[1]]
  foofoo <- foo$train(task$clone()$filter(train_set))[[1]]
  head(foofoo$data())
  
}

# CREATE GRAPH LEARNERS --------------------------------------------------------

# Create AutoML pipeline that allows to choose between a penalized logistic
# regression and a random forest classifier and tunes selected hyperparameters

# Define learners

learners <- list(glmnet = "classif.glmnet", ranger = "classif.ranger")
names_learners <- c("glmnet", "ranger")

# Create graph learners

graph_learners <- lapply(
  
  preproc_pipelines,
  
  function(i) {
    
    # Create branching pipeop
    
    po_branch_learners <- mlr3pipelines::po(
      "branch",
      options = names_learners,
      id = "choose_learner")
    
    # Create pipeop for each learner
    
    po_learners <- lapply(
      names(learners), 
      function(j) mlr3::lrn(learners[[j]], id = j))
    
    # Fix hyperparameters not included in tuning process
    
    invisible(lapply(
      
      po_learners,
      
      function(j) {
        
        params <- switch(
          j$id,
          glmnet = list(
            maxit = 10^3L,
            parallel = ifelse(parallel::detectCores() > 1L, TRUE, FALSE)),
          ranger = list(
            num.trees = 10L,
            min.node.size = ceiling(0.1 * task$nrow),
            oob.error = FALSE,
            num.threads = parallel::detectCores()))
        
        j$param_set$values <- params
        
      }))
    
    # Add learner-specific branches
    
    graph <- i %>>% 
      po_branch_learners %>>% 
      mlr3pipelines::gunion(po_learners)
    
    # Unbranch afterwards
    
    graph <- graph %>>%
      mlr3pipelines::po("unbranch")
    
    # Create graph learner
    
    mlr3pipelines::GraphLearner$new(
      graph,
      id = "graph_learner")

  }
)

# DEFINE TUNING SEARCH SPACES --------------------------------------------------

# Instantiate tuning search space with learner selection hyperparameter

hyperparameters <- paradox::ParamSet$new(list(
  paradox::ParamFct$new(
    "choose_learner.selection", 
    levels = names_learners)))

# Add learner hyperparameters

invisible(sapply(
  
  names_learners,
  
  function(i) {
    
    hyperparameters_learners <- switch(
      i,
      glmnet = paradox::ParamSet$new(list(
        paradox::ParamDbl$new(
          "glmnet.alpha", 
          lower = 0L, 
          upper = 1L))),
      log_reg = paradox::ParamSet$new(list()),
      ranger = paradox::ParamSet$new(list(
        paradox::ParamInt$new(
          "ranger.mtry",
          lower = 1L,
          upper = ceiling(0.5 * n_cols_effective)))),
      svm = paradox::ParamSet$new(list(
        paradox::ParamFct$new(
          "svm.kernel",
          levels = c("radial")),
        paradox::ParamDbl$new(
          "svm.gamma", 
          lower = 1e-4, 
          upper = 1e-1))))
    
    hyperparameters$add(hyperparameters_learners)
    
  }
))

















# Define pipeops for different classifiers

po_learners <- list(
  forest = mlr3pipelines::PipeOpLearner$new(
    learner = lrn("classif.ranger"), 
    id = "classify_forest"),
  svm = mlr3pipelines::PipeOpLearner$new(
    learner = lrn("classif.svm"), 
    id = "classify_svm"),
  lasso = mlr3pipelines::PipeOpLearner$new(
    learner = lrn("classif.cv_glmnet"), 
    id = "classify_lasso"))

# Create full graphs

graphs_full <- lapply(po_learners, function(i) graph_preproc %>>% i)

# Create graph_learners

graph_learners <- lapply(
  seq_along(graphs_full),
  function(i) GraphLearner$new(graphs_full[[i]], id = names(po_learners)[i]))
names(graph_learners) <- names(po_learners)

# BENCHMARK LEARNERS -----------------------------------------------------------

# Define search spaces

if (topic_type == "stm") {
  
  invisible(lapply(
    graph_learners,
    function(i) {
      i$param_set$values$extract_topics_stm.K <- paradox::to_tune(5L, 10L)}))
  
}

invisible(lapply(
  graph_learners,
  function(i) {
    i$param_set$values$make_glove_embeddings.dimension <- 
      paradox::to_tune(5L, 10L)}))
  
graph_learners$forest$param_set$values$classify_forest.mtry <- 
  paradox::to_tune(7L, 30L)

graph_learners$svm$param_set$values$classify_svm.type <- "C-classification"
graph_learners$svm$param_set$values$classify_svm.kernel <- 
  paradox::to_tune(c("polynomial", "radial"))
graph_learners$svm$param_set$values$classify_svm.gamma <- 
  paradox::to_tune(1e-3, 1e-1)

graph_learners$lasso$param_set$values$classify_lasso.alpha <- 
  paradox::to_tune(1e-2, 1L)

for (i in graph_learners) print(i$param_set$search_space())

# Define tuning parameters

resampling_inner <- mlr3::rsmp("cv", folds = 2L)
measure_inner <- mlr3::msr("classif.acc")
terminator <- mlr3tuning::trm("evals", n_evals = 2L)
tuner <- mlr3tuning::tnr("grid_search", resolution = 2L)

# Define tuning learners

auto_tuners <- lapply(
  graph_learners, 
  function(i) {
    mlr3tuning::AutoTuner$new(
      learner = i,
      resampling = resampling_inner,
      measure = measure_inner,
      search_space = i$param_set$search_space(),
      terminator = terminator,
      tuner = tuner)})

if (FALSE) {
  
  auto_tuners[[1]]$train(task, row_ids = train_set)
  res_cart <- auto_tuners[[1]]$predict(task, row_ids = test_set)
  res_cart$confusion
  
}

# Define benchmarking parameters

resampling_outer <- mlr3::rsmp("cv", folds = 2L)

# Define benchmarking design

benchmark_design <- mlr3::benchmark_grid(
  task$clone()$filter(train_set), 
  auto_tuners, 
  resampling_outer)

# Run benchmark 

future::plan("multiprocess")
set.seed(123L)
benchmark <- mlr3::benchmark(benchmark_design)

benchmark_results <- data.table::as.data.table(benchmark)

save_rdata_files(
  benchmark_results, 
  folder = "2_code/1_data/2_tmp_data",
  tmp = FALSE)

perf_benchmark <- benchmark$score(msr("classif.acc", id = "accuracy"))
winner <- which.max(perf_benchmark$accuracy)

# EVALUATE PERFORMANCE OF SELECTED LEARNER -------------------------------------

winning_learner <- benchmark_results$learner[[winner]]
winning_learner$train(task, row_ids = train_set)
winning_learner$param_set$values

perf_selected_learner <- winning_learner$predict(task, row_ids = test_set)

perf_selected_learner$confusion
perf_selected_learner$score(msr("classif.acc"))

# TRAIN SELECTEDLEARNER ON ENTIRE DATA -----------------------------------------

winning_learner$train(task)
perf_new_data <- winning_learner$predict_newdata(data_unlabeled)
table(perf_new_data$response)
