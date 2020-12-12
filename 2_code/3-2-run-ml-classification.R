# ------------------------------------------------------------------------------
# SENTIMENT ANALYSIS: MEDIUM APPROACH USING STANDARD MACHINE LEARNING
# ------------------------------------------------------------------------------

# Purpose: perform SA using different machine learning classifiers

# Steps:

# 1. Create learners
# 2. Train learners
# 3. Evaluate learners
# 3. Classify sentiments

# STEP 1: CREATE LEARNERS ------------------------------------------------------

load(here("2_code/1_preprocessing", "task.RData"))
load(here("2_code/1_preprocessing", "preprocessing_pipeline.RData"))

# Set up data.table with all models to be trained, including the hyperparameter
# search spaces for tuning

# id: character(1)
# graph_learner: list containing graph learner from make_graph_learner()
# tuning_search_space: list of hyperparameter ranges

# TODO Check whether complicated list structure for tuning grid is really 
# necessary - list of vectors caused error!

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
      list(id = "num.trees", value = list(1L, 10L)))))
      
))

# STEP 2: TRAIN LEARNERS -------------------------------------------------------

# Perform train-test split

split <- make_train_test_split(task, ratio = 0.8)
training_data <- split$train
test_data <- split$test

# TODO Set seed (some models might be stochastic)

# Perform tuning

ml_models[, tuning_results := lapply(
  
  seq_along(ml_models$id),
  function(m) {
    list(tune_graph_learner(
      graph_learner = ml_models$graph_learner[[m]],
      task = training_data,
      outer_resampling = mlr3::rsmp("cv", folds = 2L),
      inner_resampling = mlr3::rsmp("holdout"),
      outer_loss = mlr3::msr("classif.ce"),
      inner_loss = mlr3::msr("classif.ce"),
      hyperparameter_ranges = ml_models$tuning_search_space[[m]],
      tuning_iterations = 1L))
    
  })
]

tuning_results <- lapply(
  seq_along(ml_models), 
  function(m) {
    tune_graph_learner(
      graph_learner = ml_models[[m]]$learner,
      task = training_data,
      outer_resampling = mlr3::rsmp("cv", folds = 5L),
      inner_resampling = mlr3::rsmp("holdout"),
      outer_loss = mlr3::msr("classif.ce"),
      inner_loss = mlr3::msr("classif.ce"),
      hyperparameter_ranges = hyperparameter_ranges[[m]],
      tuning_iterations = 1L)
  })

# Set model hyperparameters according to tuning results and train on training 
# data

ml_models_tuned_trained <- lapply(
  seq_along(ml_models),
  function(m) {
    train_final_graph_learner(
      ml_models[[m]]$learner, 
      tuning_results[[m]], 
      training_data)
  })

# STEP 3: EVALUATE LEARNERS ----------------------------------------------------

predictions <- lapply(
  seq_along(ml_models_tuned_trained),
  function(m) ml_models_tuned_trained[[m]]$predict(test_data)
)

for (m in seq_along(predictions)) {
  
  print(predictions[[m]]$confusion)
  print(predictions[[m]]$score())
  
} 

# STEP 4: CLASSIFY SENTIMENTS --------------------------------------------------

data_unlabeled <- copy(data_processed)[
  , fake_label := factor(
    sample(
      c("negative", "positive"), 
      size = nrow(data_processed),
      replace = TRUE))]

task_unlabeled <- make_classification_task(
  task_name = "tweets_unlabeled",
  data = data_unlabeled,
  feature_columns = list(
    "full_text", 
    "retweet_count", 
    "favorite_count",
    "followers_count"),
  target_column = "fake_label"
)

my_sample <- sample(task_unlabeled$nrow, 100)
test_task <- task_unlabeled$clone()$filter(my_sample)

data_classified_ml_based <- copy(data_unlabeled)

data_classified_ml_based[
  , sapply(seq_along(ml_models_tuned_trained), function(m) {
    paste0("label_", tail(
        unlist(stringr::str_split(ml_models_tuned_trained[[m]]$id, "\\.")),
        1))
    }) := lapply(seq_along(ml_models_tuned_trained), function(m) {
            ml_models_tuned_trained[[m]]$predict(task_unlabeled)$response
          })]
