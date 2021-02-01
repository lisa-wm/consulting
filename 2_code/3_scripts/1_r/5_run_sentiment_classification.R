# ------------------------------------------------------------------------------
# SENTIMENT CLASSIFICATION
# ------------------------------------------------------------------------------

# IN: data with twitter-specific features and topic labels
# OUT: data with sentiment predictions

# MAKE CLASSIFICATION TASK -----------------------------------------------------

load_rdata_files(tweets_sa, folder = "2_code/1_data/2_tmp_data")

task <- make_classification_task(
  task_name = "tweets",
  data = tweets_sa[label != "none"],
  feature_columns = list(
    names(tweets_sa)[!names(tweets_sa) %in% c("doc_id", "text", "label")]),
  target_column = "label")

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

cols_numeric <- names(tweets_sa_fake)[sapply(tweets_sa_fake, is.numeric)]
cols_numeric <- cols_numeric[cols_numeric != "topic_label_stm"]
colSums(tweets_sa_fake[, ..cols_numeric], na.rm = TRUE)

tweets_sa_fake <- na.omit(tweets_sa_fake)

task <- make_classification_task(
  task_name = "tweets",
  data = tweets_sa_fake,
  feature_columns = list(
    names(tweets_sa_fake)[!names(tweets_sa_fake) %in% c("doc_id", "text", "label")]),
  target_column = "label")

graph <- mlr3pipelines::mlr_pipeops$get("branch", c("foo", "poo")) %>>%
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

graph$plot(html = FALSE)

glrn <- GraphLearner$new(graph)
glrn$param_set$values$svm.classif.svm.type <- "C-classification"

ps <- ParamSet$new(list(
  ParamDbl$new("svm.classif.svm.cost", lower = 0, upper = 0.5),
  ParamInt$new("rf.classif.ranger.num.trees", lower = 1L, upper = 10L)
))

instance <- TuningInstanceSingleCrit$new(
  task = task,
  learner = glrn,
  resampling = rsmp("cv", folds = 3L),
  measure = msr("classif.ce"),
  search_space = ps,
  terminator = trm("evals", n_evals = 3L)
)

tuner <- tnr("random_search")
tuner$optimize(instance)

instance$result_learner_param_vals
instance$result_y

# SET UP LIST OF LEARNERS ------------------------------------------------------

learners <- list(
  
  ids = c("svm", "rf"),
  
  learners = list(
    svm = make_graph_learner(
      learner_type = "svm",
      learner_params = list(
        kernel = "radial",
        type = "C-classification"),
      learner_name = "support_vector_machine"),
    rf = make_graph_learner(
      learner_type = "ranger",
      learner_params = list(),
      learner_name = "random_forest")),
  
  hyperparameter_ranges = list(
    svm = list(
      tol = list(
        id = "tolerance", 
        ranges = c(0.001, 0.003)),
      type = list(
        id = "type",
        ranges = list("C-classification", "nu-classification"))),
    rf = list(
      tol = list(
        id = "tolerance",
        ranges = c(0.001, 0.003)),
      n_trees = list(
        id = "num.trees", 
        ranges = c(1L, 10L)))))

# TUNE LEARNERS ----------------------------------------------------------------

learners$tuning_results <- lapply(
  seq_along(learners$ids),
  function(i) {
    this_learner <- learners$ids[i]
    tune_graph_learner(
      graph_learner = learners$learners[[this_learner]],
      task = task,
      outer_resampling = mlr3::rsmp("cv", folds = 3L),
      inner_resampling = mlr3::rsmp("cv", folds = 3L),
      outer_loss = mlr3::msr("classif.ce"),
      inner_loss = mlr3::msr("classif.ce"),
      hyperparameter_ranges = learners$hyperparameter_ranges[[this_learner]],
      tuning_iterations = 1L)})

learners[, tuning_results := lapply(
  .I,
  function(m) {list(tuning_result = tune_graph_learner(
    graph_learner = graph_learner[[m]],
    task = task,
    outer_resampling = mlr3::rsmp("cv", folds = 3L),
    inner_resampling = mlr3::rsmp("cv", folds = 3L),
    outer_loss = mlr3::msr("classif.ce"),
    inner_loss = mlr3::msr("classif.ce"),
    hyperparameter_ranges = tuning_search_space[[m]],
    tuning_iterations = 1L))})]

# ------------------------------------------------------------------------------
# SENTIMENT ANALYSIS: MEDIUM APPROACH USING STANDARD MACHINE LEARNING
# ------------------------------------------------------------------------------

# Purpose: perform SA using different machine learning classifiers

# Steps:

# 1. Create preprocessing pipeline
# 2. Create learners
# 3. Train learners
# 4. Evaluate learners
# 5. Classify sentiments

# STEP 1: CREATE PREPROCESSING PIPELINE ----------------------------------------

# Conveniently, mlr3 works with quanteda for preprocessing texts.
# Therefore, the preprocessing step carried out the dictionary-based approach
# can be replicated and encapsulated in a graph.
# This way, in the nested resampling procedure performed to tune and evaluate
# learners, preprocessing is done anew for every fold, ensuring no information
# from training data leaks into test sets.

# Create mlr3 task

load(here("2_code/0_training_data", "rdata-training-data-annotated.RData"))

task <- make_classification_task(
  task_name = "tweets",
  data = training_data_annotated,
  feature_columns = list(
    "full_text", 
    "retweet_count", 
    "favorite_count",
    "followers_count"),
  target_column = "label"
)

save(
  task, 
  file = here("2_code/3_sentiment_analysis/2_ml_based", "rdata-ml-task.RData"))

# Create mlr3 graph, where one branch takes care of preprocessing the text
# column while the other just passes the remaining features on

preprocessing_pipeline <- make_preprocessing_pipeline(
  text_column = "full_text",
  n = 1L,
  min_termfreq = 1L,
  stopwords = make_stopwords()
)

preprocessing_pipeline$plot(html = FALSE)

# Save pipeline to be used for machine learning classifiers

save(
  preprocessing_pipeline,
  file = here(
    "2_code/3_sentiment_analysis/2_ml_based", 
    "rdata-preprocessing-pipeline.RData"))

# STEP 2: CREATE LEARNERS ------------------------------------------------------

# Set up data.table with all models to be trained, including the hyperparameter
# search spaces for tuning

# id: character(1)
# graph_learner: list containing graph learner from make_graph_learner()
# tuning_search_space: list of hyperparameter ranges

# TODO Fix that gruesome structure of umpteen nested lists
# (as.list covers only first element, list of vectors for ranges caused some
# weird error in resample)

ml_models <- rbindlist(list(
  
  data.table(
    id = "svm",
    graph_learner = list(learner = make_graph_learner(
      learner_type = "svm",
      learner_params = list(
        kernel = "radial",
        type = "C-classification"),
      learner_name = "support_vector_machine"),
    tuning_search_space = list(list(
      list(id = "tolerance", value = list(0.001, 0.003)),
      list(id = "type", value = list("C-classification", "nu-classification"))))
  )),

  data.table(
    id = "rf",
    graph_learner = list(learner = make_graph_learner(
      learner_type = "ranger",
      learner_params = list(),
      learner_name = "random_forest"),
    tuning_search_space = list(list(
      list(id = "num.trees", value = list(1L, 10L)))))
      
)))

# STEP 2: TRAIN LEARNERS -------------------------------------------------------

# Perform train-test split

split <- make_train_test_split(task, ratio = 0.8)
training_data <- split$train
test_data <- split$test

# TODO Set seed (some models might be stochastic)

# Perform tuning

ml_models[, tuning_results := lapply(
  .I,
  function(m) {list(tuning_result = tune_graph_learner(
      graph_learner = graph_learner[[m]],
      task = training_data,
      outer_resampling = mlr3::rsmp("cv", folds = 2L),
      inner_resampling = mlr3::rsmp("holdout"),
      outer_loss = mlr3::msr("classif.ce"),
      inner_loss = mlr3::msr("classif.ce"),
      hyperparameter_ranges = tuning_search_space[[m]],
      tuning_iterations = 1L))})]

# Set model hyperparameters according to tuning results and train on training 
# data

ml_models[, graph_learner_tuned := lapply(
  .I,
  function(m) {train_final_graph_learner(
      graph_learner = graph_learner[[m]],
      tuning_result = tuning_results[[m]]$tuning_result,
      training_data
    )})]

# STEP 3: EVALUATE LEARNERS ----------------------------------------------------

ml_models[, predictions_test_data := lapply(
  .I,
  function(m) graph_learner_tuned[[m]]$predict(test_data))]

ml_models[, `:=` (
  score_test_data = lapply(
    .I,
    function(m) predictions_test_data[[m]]$score()),
  confusion_test_data = lapply(
    .I,
    function(m) predictions_test_data[[m]]$confusion))]

# STEP 4: CLASSIFY SENTIMENTS --------------------------------------------------

# As preprocessing is part of the pipeline, predictions can be made on the 
# processed data directly (i.e., no need for a dfm object as in dictionary-based
# approach)

load(here("2_code", "rdata-tweets-processed.RData"))

data_classified_ml_based <- copy(data_processed)[
  , sapply(
    seq_along(ml_models$id), 
    function(m) {
      paste0("prob_pos_", ml_models$id[[m]])
    }) :=
    lapply(
      seq_along(ml_models$id), 
      function(m) {
        ml_models$graph_learner_tuned[[m]]$predict_newdata(data_processed)$
          prob[, "positive"]})]

data_classified_ml_based[
  , sapply(
    seq_along(ml_models$id), 
    function(m) {
      paste0("label_", ml_models$id[[m]])
    }) := lapply(.SD, function(i) ifelse(i >= 0.5, 1, 0)), 
  .SDcols = names(data_classified_ml_based)[
    grep("prob", names(data_classified_ml_based))]
]

save(
  data_classified_ml_based,
  file = here("2_code", "rdata-tweets-labeled-ml.RData"))
