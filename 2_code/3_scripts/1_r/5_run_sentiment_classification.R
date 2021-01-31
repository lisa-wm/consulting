# ------------------------------------------------------------------------------
# SENTIMENT CLASSIFICATION
# ------------------------------------------------------------------------------

# IN: data with topic labels
# OUT: data with twitter-specific features

# EXTRACT TWITTER-SPECIFIC FEATURES --------------------------------------------







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
      learner_name = "support_vector_machine",
      preprocessing_pipeline = preprocessing_pipeline)$learner),
    tuning_search_space = list(list(
      list(id = "tolerance", value = list(0.001, 0.003)),
      list(id = "type", value = list("C-classification", "nu-classification"))))
  ),

  data.table(
    id = "rf",
    graph_learner = list(learner = make_graph_learner(
      learner_type = "ranger",
      learner_params = list(),
      learner_name = "random_forest",
      preprocessing_pipeline = preprocessing_pipeline)$learner),
    tuning_search_space = list(list(
      list(id = "num.trees", value = list(1L, 10L))))),
  
  data.table(
    id = "naive_bayes",
    graph_learner = list(learner = make_graph_learner(
      learner_type = "naive_bayes",
      learner_params = list(),
      learner_name = "naive_bayes",
      preprocessing_pipeline = preprocessing_pipeline)$learner),
    tuning_search_space = NA)
      
))

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
