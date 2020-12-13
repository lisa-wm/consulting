# ------------------------------------------------------------------------------
# TRAINING GRAPH LEARNERS
# ------------------------------------------------------------------------------

# Purpose: set hyperparameters according to tuning results and train on training
# data

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

train_final_graph_learner <- function(graph_learner, 
                                      tuning_result, 
                                      training_data) {
  
  # Otherwise, set 
  
  set.seed(1)
  
  # If tuning has been performed, set parameters to tuned parameters
  
  if (any(class(tuning_result) == "ResampleResult")) {
    graph_learner$param_set$values <- 
      tuning_result$learners[[1]]$model$tuning_instance$
      result_learner_param_vals
  }
  
  graph_learner$train(training_data)
  graph_learner
  
}
