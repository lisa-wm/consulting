# ------------------------------------------------------------------------------
# CREATION OF LEARNER OBJECT
# ------------------------------------------------------------------------------

# PURPOSE: create graph learner to perform sentiment analysis in mlr3

make_graph_learner <- function(learner_type, learner_params, learner_name) {
  
  # Perform basic input checks
  
  checkmate::assert_string(learner_type)
  checkmate::assert_list(learner_params)
  
  learner_id <- sprintf("classif.%s", learner_type)
  
  if (!mlr3::mlr_learners$has(learner_id)) {
    stop(paste0(
      sprintf("%s is not a learner supported by mlr3. ", learner_type),
      "For a dictionary of supported learners, call mlr3::mlr_learners"))
  }
  
  # Make graph and graph learner
  
  learner <- mlr3::lrn(learner_id, predict_type = "prob")
  learner$param_set$values = learner_params
  
  # graph <- preprocessing_pipeline %>>% learner
  # graph_learner <- GraphLearner$new(learner, id = learner_name)
  # 
  # list(graph = graph, learner = graph_learner)
  
  GraphLearner$new(learner, id = learner_name)
  
}
