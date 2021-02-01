# ------------------------------------------------------------------------------
# SENTIMENT CLASSIFICATION
# ------------------------------------------------------------------------------

# IN: data with twitter-specific features and topic labels
# OUT: data with sentiment predictions

# MAKE CLASSIFICATION TASK -----------------------------------------------------

load_rdata_files(tweets_sa, folder = "2_code/1_data/2_tmp_data")

tweets_sa_fake <- copy(tweets_sa)[
  , label := ifelse(
    label == "none" & positive_emojis > 0,
    "positive",
    label)
  ][, label := ifelse(
    label == "none" & negative_emojis > 0,
    "negative",
    label)
    ][, label := as.factor(label)
      ][label != "none"]

tweets_sa_fake <- na.omit(tweets_sa_fake)

task <- make_classification_task(
  task_name = "tweets",
  data = tweets_sa_fake,
  feature_columns = list(
    names(tweets_sa_fake)[!names(tweets_sa_fake) %in% c("doc_id", "text", "label")]),
  target_column = "label")

# MAKE GRAPH LEARNER -----------------------------------------------------------

graph <- mlr3pipelines::mlr_pipeops$get("branch", c("svm", "rf")) %>>%
  gunion(list(
    mlr_pipeops$get(
      "learner", 
      learner = PipeOpLearner$new(mlr_learners$get("classif.svm")), 
      id = "svm"),
    mlr_pipeops$get(
      "learner", 
      learner = PipeOpLearner$new(mlr_learners$get("classif.ranger")), 
      id = "rf")
  )) %>>%
  mlr_pipeops$get("unbranch")

graph <- ppl(
  "branch", 
  list(
    svm = lrn(
      "classif.svm", 
      id = "svm",
      kernel = "radial",
      type = "C-classification"),
    rf = lrn(
      "classif.ranger",
      id = "rf")),
  prefix_branchops = "lrn_")

graph$plot(html = FALSE)

glrn <- GraphLearner$new(graph)

# TUNE GRAPH LEARNER -----------------------------------------------------------

ps <- ParamSet$new(list(
  ParamFct$new("lrn_branch.selection", levels = c("svm", "rf")),
  ParamDbl$new("svm.cost", lower = 0, upper = 0.5),
  ParamInt$new("rf.num.trees", lower = 1L, upper = 10L)))

ps$add_dep("svm.cost", "lrn_branch.selection", CondEqual$new("svm"))
ps$add_dep("rf.num.trees", "lrn_branch.selection", CondEqual$new("rf"))

resampling_inner <- rsmp("cv", folds = 3L)
measure_inner <- msr("classif.ce")
search_space <- ps
tuner <- tnr("random_search")
terminator <- trm("evals", n_evals = 3L)

resampling_outer <- rsmp("cv", folds = 3)
measure_outer <- msr("classif.ce")

auto_tuner <- AutoTuner$new(
  learner = glrn,
  resampling = resampling_inner,
  measure = measure_inner,
  search_space = search_space,
  tuner = tuner,
  terminator = terminator)

tuning_result <- resample(
  task = task, 
  learner = auto_tuner, 
  resampling = resampling_outer)


inst = TuningInstanceSingleCrit$new(tsk("sonar"), glrn, rsmp("cv", folds=3),
                                    msr("classif.ce"), ps, trm("evals", n_evals = 10))
tnr("random_search")$optimize(inst)





tuning_result <- lapply(
  
  c("svm", "rf"),
  
  function(i) {
    
    glrn$param_set$values$branch.selection <- i
    
    resampling_inner <- rsmp("cv", folds = 3L)
    measure_inner <- msr("classif.ce")
    search_space <- ps[[i]]
    tuner <- tnr("random_search")
    terminator <- trm("evals", n_evals = 3L)
    
    resampling_outer <- rsmp("cv", folds = 3)
    measure_outer <- msr("classif.ce")
    
    auto_tuner <- AutoTuner$new(
      learner = glrn,
      resampling = resampling_inner,
      measure = measure_inner,
      search_space = search_space,
      tuner = tuner,
      terminator = terminator)
    
    resample(
      task = task, 
      learner = auto_tuner, 
      resampling = resampling_outer)
    
  })

names(tuning_result) <- learners$ids

at = AutoTuner$new(
  learner = learner,
  resampling = resampling,
  measure = measure,
  search_space = search_space,
  terminator = terminator,
  tuner = tuner
)

ge_estimates <- sapply(
  c("svm", "rf"), 
  function(i) tuning_result[[i]]$aggregate())

best_classifier <- c("svm", "rf")[which.min(ge_estimates)]

hp_best_classifier <- tuning_result[[best_classifier]]$learner$tuning_result
