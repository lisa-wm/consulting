# ------------------------------------------------------------------------------
# SENTIMENT CLASSIFICATION PIPELINE
# ------------------------------------------------------------------------------

# IN: data filtered for subjective tweets
# OUT: data with sentiment labels

task <- tsk("iris")

PipeOpScaleAlwaysSimple = R6::R6Class(
  "PipeOpScaleAlwaysSimple",
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "extract_topics_stm", 
                          param_vals = list()) {
      
      ps = ParamSet$new(params = list(
        ParamDbl$new("lhs", default = 5),
        ParamUty$new("rhs")
      ))
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
        scale = 4,
        foo = self$param_set$values$lhs
      )
    },
    
    .transform_dt = function(dt, levels) {
      dt_new <- cbind(
        dt,
        rep(self$state$foo, nrow(dt)),
        t((t(dt) - self$state$center) / self$state$scale))
      setnames(dt_new, letters[1:ncol(dt_new)])
      dt_new
    }
  )
)


pop <- PipeOpScaleAlwaysSimple$new()
pop$param_set$values$lhs <- 99
gr = Graph$new()$add_pipeop(pop)

result_posa = gr$train(task)[[1]]

result_posa$data()

gr <- gr %>>% mlr_pipeops$get("learner", learner = lrn("classif.rpart"))

glrn <- GraphLearner$new(gr)

glrn$train(task)
glrn$predict(task)

