# ------------------------------------------------------------------------------
# TASK CREATION FOR ML MODELS
# ------------------------------------------------------------------------------

# Purpose: make mlr3 pipeline machine learning classifiers

# HELPER FUNCTIONS -------------------------------------------------------------

# Select models

models <- lapply(
  c("featureless",
    "log_reg",
    "naive_bayes",
    "ranger",
    "svm",
    "xgboost"),
  function(i) paste0("classif.", i))

# Set up learners predicting probabilities

learners <- lapply(
  models, 
  lrn,
  predict_type = "prob", 
  predict_sets = c("train", "test")) 

# Define inner loss and resampling procedure

inner_loss <- list(
  msr("classif.ce", id = "mmce_train", predict_sets = "train"),
  msr("classif.ce", id = "mmce_test")
)

inner_resampling <- rsmp("cv", folds = 5)

# Define outer loss and resampling procedure



dt = data.table(
  txt = replicate(150, paste0(sample(training_data_annotated$full_text[1:3], 3), collapse = " "))
)
task = tsk("iris")$cbind(dt)

pos = po("textvectorizer", param_vals = list(stopwords_language = "de"))

pos$train(list(task))[[1]]$data()

one_line_of_iris = task$filter(13)

one_line_of_iris$data()

pos$predict(list(one_line_of_iris))[[1]]$data()
