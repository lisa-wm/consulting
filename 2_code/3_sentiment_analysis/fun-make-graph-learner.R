# ------------------------------------------------------------------------------
# CREATING GRAPH LEARNERS
# ------------------------------------------------------------------------------

# Purpose: make mlr3 graph learner for machine learning classifiers

# HELPER FUNCTIONS -------------------------------------------------------------

make_learner <- function(learner_id, learner_params) {
  
  learner <- mlr3::lrn(learner_id, predict_type = "prob")
  learner$param_set$values = learner_params
  learner
  
}

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

make_graph_learner <- function(preprocessing_pipeline, 
                               learner_type, 
                               learner_params,
                               learner_name) {
  
  # Perform basic input checks
  
  checkmate::assert_string(learner_type)
  checkmate::assert_list(learner_params)
  
  learner_id <- paste0("classif.", learner_type)
  
  if (!mlr3::mlr_learners$has(learner_id)) {
    stop(paste0(
      sprintf("%s is not a learner supported by mlr3. ", learner_type),
      "For a dictionary of supported learners, call mlr3::mlr_learners"))
  }
  
  # Make graph and graph learner
  
  learner <- make_learner(learner_id, learner_params)
  graph <- preprocessing_pipeline %>>% learner
  graph_learner <- GraphLearner$new(graph, id = learner_name)
  
  list(graph = graph, learner = graph_learner)
  
}
