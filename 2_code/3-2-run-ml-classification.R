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

ml_models <- list(
  svm = make_graph_learner(
    learner_type = "svm",
    learner_params = list(
      kernel = "radial",
      type = "C-classification"),
    learner_name = "support_vector_machine",
    preprocessing_pipeline = preprocessing_pipeline),
  rf = make_graph_learner(
    learner_type = "ranger",
    learner_params = list(),
    learner_name = "random_forest",
    preprocessing_pipeline = preprocessing_pipeline))

# STEP 2: TRAIN LEARNERS -------------------------------------------------------

# Perform train-test split

split <- make_train_test_split(task, ratio = 0.8)
training_data <- split$train
test_data <- split$test

# TODO Outsource in function
# TODO Set seed (some models might be stochastic)

# Define grid for hyperparameters

hyperparameter_ranges <- list(
  svm = list(
    list("cost", value = list(1L, 2L))),
  rf = list(
    list("num.trees", value = list(1L, 10L)),
    list("min.node.size", value = list(1L, 2L)))
)

# Perform tuning

tuning_results <- lapply(
  seq_along(ml_models), 
  function(m) {train_graph_learner(
    graph_learner = ml_models[[m]]$learner,
    task = training_data,
    outer_resampling = mlr3::rsmp("cv", folds = 5L),
    inner_resampling = mlr3::rsmp("holdout"),
    outer_loss = mlr3::msr("classif.auc"),
    inner_loss = mlr3::msr("classif.ce"),
    hyperparameter_ranges = hyperparameter_ranges[[m]],
    tuning_iterations = 1L)}
)

# Set model hyperparameters according to tuning results

ml_models_tuned <- ml_models

for (m in seq_along(ml_models_tuned)) {
  
  ml_models_tuned[[m]]$learner$param_set$values <- 
    tuning_results[[m]]$learners[[1]]$model$tuning_instance$
    result_learner_param_vals
  
}

# Train models on training data

lapply(
  seq_along(ml_models_tuned),
  function(m) ml_models_tuned[[m]]$learner$train(training_data))

# STEP 3: EVALUATE LEARNERS ----------------------------------------------------

predictions <- lapply(
  seq_along(ml_models_tuned),
  function(m) ml_models_tuned[[m]]$learner$predict(test_data)
)

for (m in seq_along(predictions)) print(predictions[[m]]$confusion)

# STEP 4: CLASSIFY SENTIMENTS --------------------------------------------------

