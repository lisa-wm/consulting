# ------------------------------------------------------------------------------
# EXTENDING THE PREPROCESSING GRAPH
# ------------------------------------------------------------------------------

# Purpose: make mlr3 pipeline machine learning classifiers

# HELPER FUNCTIONS -------------------------------------------------------------

model_list <- list(
  
  "random" = lrn(
    "classif.featureless",
    id = "random_classifier"
  ),
  "logreg" = lrn(
    "classif.log_reg",
    id = "log_regr"
  ),
  "svm" = lrn(
    "classif.svm",
    id = "svm",
    kernel = "radial",
    type = "C-classification"),
  "xgb" = lrn(
    "classif.xgboost",
    id = "xgb"),
  "rf" = lrn(
    "classif.ranger", 
    id = "rf")
)

graph_learners <- list(length(model_list))

for (m in seq_along(model_list)) {
  
  this_graph <- preprocessing_ppl %>>%
    model_list[[m]]
  
  graph_learners[[model_list[[m]]$id]] <- GraphLearner$new(this_graph)
  
}

graph_learners[["random_classifier"]]$train(task)
pred <- graph_learners[["random_classifier"]]$predict(task)
pred$confusion

# Define inner loss and resampling procedure

inner_loss <- list(
  msr("classif.ce", id = "mmce_train", predict_sets = "train"),
  msr("classif.ce", id = "mmce_test")
)

inner_resampling <- rsmp("cv", folds = 5)

# Define outer loss and resampling procedure
