# ------------------------------------------------------------------------------
# SENTIMENT CLASSIFICATION PIPELINE
# ------------------------------------------------------------------------------

# IN: corpus object of cleaned tweets and meta data
# OUT: data with sentiment labels

# SET TUNING & BENCHMARKING HYPERPARAMETERS ------------------------------------

resampling_strategy_inner <- mlr3::rsmp("cv", folds = 3L)
measure_inner <- mlr3::msr("classif.acc")
tuner <- mlr3tuning::tnr("random_search")
terminator <- mlr3tuning::trm("evals", n_evals = 50L)
resampling_strategy_outer <- mlr3::rsmp("cv", folds = 3L)

# MAKE CLASSIFICATION TASK -----------------------------------------------------

# Load data

load_rdata_files(
  tweets_corpus_features, 
  folder = "2_code/1_data/2_tmp_data",
  tmp = FALSE)

tweets_sa <- convert_qtda_to_dt(tweets_corpus_features, key = "doc_id")

cols_to_keep <- c(
  "doc_id",
  "label",
  "text",
  names(tweets_sa)[startsWith(names(tweets_sa), "feat")],
  "meta_party",
  "meta_bundesland",
  "meta_unemployment_rate",
  "meta_share_pop_migration",
  "twitter_username",
  "twitter_year",
  "twitter_month")

char_cols <- sprintf("feat_%s", letters)
tweets_sa <- tweets_sa[, ..cols_to_keep]

tweets_sa[
  , doc_group := paste(
    twitter_username, 
    twitter_year, 
    twitter_month, 
    sep = "_")]

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
  doc_grouping_var = "doc_group",
  prevalence_vars_cat = list("meta_party", "meta_bundesland"),
  prevalence_vars_smooth = list(
    "meta_unemployment_rate", 
    "meta_share_pop_migration"),
  max.em.its = 10L,
  stopwords = make_stopwords(),
  init.type = "LDA")

preproc_pipelines <- list(
  ppl_with_tm = mlr3pipelines::Graph$new()$add_pipeop(po_tm), 
  ppl_without_tm = mlr3pipelines::Graph$new()$add_pipeop(
    mlr3pipelines::po("nop", id = "start")))

# Create pipelines

preproc_pipelines <- lapply(
  
  names(preproc_pipelines),
  
  function(i) {
    
    # Create glove embedding pipeop (defined in separate function)
    
    po_embeddings <- PipeOpMakeGloveEmbeddings$new()
    po_embeddings$param_set$values$stopwords <- make_stopwords()

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
            min.node.size = ceiling(0.05 * task$nrow),
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

plot_graph <- plot(graph_learners[[1]]$graph)

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

# set.seed(123L)
# invisible(lapply(auto_tuners, function(i) i$train(task)))
# 
# save_rdata_files(auto_tuners, folder = "2_code/1_data/2_tmp_data", tmp = FALSE)

# TRAIN FINAL MODELS -----------------------------------------------------------

# invisible(lapply(
#   seq_along(graph_learners),
#   function(i) {
#     graph_learners[[i]]$param_set$values <- 
#       auto_tuners[[i]]$tuning_instance$result_learner_param_vals
#     graph_learners[[i]]$train(task)}))
# 
# save_rdata_files(
#   graph_learners, 
#   folder = "2_code/1_data/2_tmp_data", 
#   tmp = FALSE)
# 
# graph_learners$ppl_with_tm$param_set$values
# graph_learners$ppl_without_tm$param_set$values

# predictions <- lapply(
#   graph_learners,
#   function(i) i$predict_newdata(data_unlabeled[complete.cases(data_unlabeled)]))
# 
# table(pred$response)
# pred$predict_types

# EVALUATE PERFORMANCE ---------------------------------------------------------

# Set up baseline: classifier that ignores features

learner_featureless <- mlr3::lrn("classif.featureless")
learners_with_baseline <- append(auto_tuners, learner_featureless)
names(learners_with_baseline) <- c(names(graph_learners), "featureless")

invisible(lapply(
  seq_along(learners_with_baseline), 
  function(i) {
    learners_with_baseline[[i]]$id <- names(learners_with_baseline)[i]}))

# Calculate benchmark via nested resampling

benchmark_design = mlr3::benchmark_grid(
  tasks = task,
  learners = learners_with_baseline,
  resamplings = resampling_strategy_outer)

set.seed(123L)
benchmark_results <- mlr3::benchmark(benchmark_design, store_models = TRUE)

save_rdata_files(
  benchmark_results, 
  folder = "2_code/1_data/2_tmp_data",
  tmp = FALSE)

load_rdata_files(
  benchmark_results,
  folder = "2_code/1_data/2_tmp_data",
  tmp = FALSE)

# Evaluate (swapping names as so they match the manner of encoding)

metrics <- list(
  acc = mlr3::msr("classif.acc", id = "acc"),
  tn = mlr3::msr("classif.tn", id = "tp"),
  tp = mlr3::msr("classif.tp", id = "tn"),
  fn = mlr3::msr("classif.fn", id = "fp"),
  fp = mlr3::msr("classif.fp", id = "fn"))

evaluation <- benchmark_results$aggregate(metrics)[
  , .(learner_id, acc, tn, tp, fn, fp)]

evaluation[, f_1 := 2 * (tp / (tp + fp) * tp / (tp + fn)) / 
             (tp / (tp + fp) + tp / (tp + fn))]

round(evaluation[, .(acc, f_1, tn, tp, fn, fp)], 3L)

# ANALYZE RESULTS --------------------------------------------------------------

bmr_learner_w_tm <- 
  benchmark_results$resample_results$resample_result[[1]]$learners

bmr_learner_wo_tm <- 
  benchmark_results$resample_results$resample_result[[2]]$learners

configs_w_tm <- sapply(
  bmr_learner_w_tm,
  function(i) i$tuning_result)

configs_wo_tm <- sapply(
  bmr_learner_wo_tm,
  function(i) i$tuning_result)

coefficients_ranger <- lapply(
  seq_len(bmr_learner_w_tm[[1]]$learner$model$ranger$model$num.trees),
  function(i) {
    ranger::treeInfo(
      bmr_learner_w_tm[[1]]$learner$model$ranger$model, 
      tree = i)})

coefficients_ranger_all <- do.call(rbind, coefficients_ranger)
sort(table(coefficients_ranger_all$splitvarName))

coefficients_glmnet <- lapply(
  unlist(list(bmr_learner_w_tm[2:3], bmr_learner_wo_tm)),
  function(i) i$learner$model$glmnet$model$beta)
