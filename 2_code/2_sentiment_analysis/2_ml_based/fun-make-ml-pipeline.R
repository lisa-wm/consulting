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

# TODO: Set up resampling

graph_learners <- list()

for (m in seq_along(model_list)) {
  
  this_graph <- preprocessing_ppl %>>%
    model_list[[m]]
  
  graph_learners[[model_list[[m]]$id]] <- GraphLearner$new(this_graph)
  
}

confusion_matrices <- list()

for (m in seq_along(graph_learners)) {
  
  graph_learners[[m]]$train(task)
  pred <- graph_learners[[m]]$predict(task)
  confusion_matrices[[model_list[[m]]$id]] <- pred$confusion
  
}
