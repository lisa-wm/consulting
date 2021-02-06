# ------------------------------------------------------------------------------
# CREATION TOPIC MODELING PIPEOP
# ------------------------------------------------------------------------------

# PURPOSE: create mlr3pipelines pipe operator for topic modeling

# MAKE PIPEOP ------------------------------------------------------------------

PipeOpExtractTopicsSTM = R6::R6Class(
  
  "PipeOpExtractTopics",
  
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  
  public = list(
    
    initialize = function(id = "extract_topics_stm", 
                          param_vals = list()) {
      
      ps = ParamSet$new(params = list(
        ParamUty$new("mutation")
      ))
      ps$values = list(mutation = list())
      super$initialize(id, ps, param_vals = param_vals)
    }
    
  ),
  
  private = list(
    .select_cols = function(task) {
      task$feature_types[type == "numeric", id]
    },
    
    .get_state_dt = function(dt, levels, target) {
      list(
        center = sapply(dt, mean),
        scale = sapply(dt, sd)
      )
    },
    
    .transform_dt = function(dt, levels) {
      t((t(dt) - self$state$center) / self$state$scale)
    }
  )
)