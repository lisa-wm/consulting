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

# Make task

data_labeled <- tweets_sa[label != "none"]
data_labeled$label <- as.factor(data_labeled$label)
data_unlabeled <- tweets_sa[label == "none"]

stopifnot(nrow(data_labeled) + nrow(data_unlabeled) == nrow(tweets_sa))

invisible(sapply(
  list(data_labeled, data_unlabeled), 
  data.table::setorder, doc_id))

task <- mlr3::TaskClassif$new(
  "sentiment_analysis", 
  backend = data_labeled,
  target = "label")

# CREATE PRE-PROCESSING PIPELINES ----------------------------------------------

# Create two different graph learners to benchmark against each other (one with,
# one without upstream topic modeling)

# Create topic modeling pipeop (defined in separate function)

po_tm <- PipeOpExtractTopicsSTM$new()

po_tm$param_set$values <- list(
  docid_field = "doc_id",
  text_field = "text",
  doc_grouping_var = c("twitter_username", "twitter_year", "twitter_month"),
  prevalence_vars_cat = list("meta_party", "meta_bundesland"),
  prevalence_vars_smooth = list("meta_unemployment_rate"),
  max.em.its = 5L,
  stopwords = make_stopwords(),
  init.type = "LDA")

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
      ppl_with_tm = mlr3pipelines::selector_name(
        c("doc_id", "topic_label", "text")),
      ppl_without_tm = mlr3pipelines::selector_name(c("doc_id", "text")))
    
    po_sel_embeddings_inv$param_set$values$selector <- switch(
      i,
      ppl_with_tm = mlr3pipelines::selector_invert(
        mlr3pipelines::selector_name(c("topic_label", "text"))),
      ppl_without_tm = mlr3pipelines::selector_invert(
        mlr3pipelines::selector_name(c("text"))))
    
    # Create glove embedding pipeop (defined in separate function)
    
    po_embeddings <- PipeOpMakeGloveEmbeddings$new()
    po_embeddings$param_set$values$stopwords <- make_stopwords()

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

    preproc_pipelines[[i]]%>>%
      po_embeddings %>>%
      po_sel_sentiment_analysis %>>%
      po_set_colroles
      
})

if (FALSE) {
  
  foo <- preproc_pipelines[[1]]
  foofoo <- foo$train(task$clone())[[1]]
  head(foofoo$data())
  phew <- foo$predict(task$clone())[[1]]
  head(phew$data())
  
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
            num.trees = 200L,
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

})

names(graph_learners) <- c("ppl_with_tm", "ppl_without_tm")

# DEFINE TUNING SEARCH SPACES --------------------------------------------------

# Instantiate tuning search space

hyperparameters <- paradox::ParamSet$new(list(
  paradox::ParamInt$new(
    "make_glove_embeddings.dimension",
    lower = 10L,
    upper = 50L),
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
          upper = 1L),
        paradox::ParamDbl$new(
          "glmnet.lambda",
          lower = 0L, 
          upper = 0.2))),
      ranger = paradox::ParamSet$new(list(
        paradox::ParamInt$new(
          "ranger.mtry",
          lower = 1L,
          upper = ceiling(0.5 * task$ncol)),
        paradox::ParamDbl$new(
          "ranger.sample.fraction",
          lower = 0.25,
          upper = 1L))))
    
    hyperparameters$add(hyperparameters_learners)
    
}))

# Add dependencies between learner selection and associated hyperparameters

invisible(sapply(
  
  names_learners,
  
  function(i) {
    
    this_hp_set <- hyperparameters$ids()[startsWith(hyperparameters$ids(), i)]
    
    for (j in this_hp_set) {
      hyperparameters$add_dep(
        j, 
        "choose_learner.selection", 
        paradox::CondEqual$new(i))}
    
}))

hyperparameters

# CONDUCT TUNING ---------------------------------------------------------------

# Define resampling strategy: 3-fold cross-validation

resampling_strategy_inner <- mlr3::rsmp("cv", folds = 3L)

# Define performance metric: accuracy

measure_inner <- mlr3::msr("classif.acc")

# Define tuner: random search

tuner <- mlr3tuning::tnr("random_search")

# Define terminator: number of evals

terminator <- mlr3tuning::trm("evals", n_evals = 20L)

# Set up autotuners for each pipeline

# future::plan("multisession")

auto_tuners <- lapply(
  
  names(graph_learners),
  
  function(i) {
    
    mlr3tuning::AutoTuner$new(
      learner = graph_learners[[i]],
      search_space = switch(
        i,
        ppl_with_tm = hyperparameters$clone()$add(
          paradox::ParamSet$new(list(
            paradox::ParamInt$new(
              "extract_topics_stm.K", 
              lower = 3L, 
              upper = 6L)))),
        ppl_without_tm = hyperparameters),
      resampling = resampling_strategy_inner,
      measure = measure_inner,
      terminator = terminator,
      tuner = tuner)
    
})

# Tune

set.seed(123L)
invisible(lapply(auto_tuners, function(i) i$train(task)))

save_rdata_files(auto_tuners, folder = "2_code/1_data/2_tmp_data", tmp = FALSE)

# TRAIN FINAL MODELS -----------------------------------------------------------

invisible(lapply(
  seq_along(graph_learners),
  function(i) {
    graph_learners[[i]]$param_set$values <- 
      auto_tuners[[i]]$tuning_instance$result_learner_param_vals
    graph_learners[[i]]$train(task)}))

save_rdata_files(
  graph_learners, 
  folder = "2_code/1_data/2_tmp_data", 
  tmp = FALSE)

graph_learners$ppl_with_tm$param_set$values
graph_learners$ppl_without_tm$param_set$values

# predictions <- lapply(
#   graph_learners,
#   function(i) i$predict_newdata(data_unlabeled[complete.cases(data_unlabeled)]))
# 
# table(pred$response)
# pred$predict_types

# EVALUATE PERFORMANCE ---------------------------------------------------------

# Define resampling strategy: 3-fold cross-validation

resampling_strategy_outer <- mlr3::rsmp("cv", folds = 3L)

# Perform nested resampling

nested_resampling_results <- lapply(
  seq_along(graph_learners),
  function(i) {
    mlr3::resample(
      task = task, 
      learner = auto_tuners[[i]], 
      resampling = resampling_strategy_outer, 
      store_models = TRUE)})

save_rdata_files(
  nested_resampling_results, 
  folder = "2_code/1_data/2_tmp_data",
  tmp = FALSE)

sapply(
  nested_resampling_results,
  function(i) i$aggregate(mlr3::msr("classif.acc")))
