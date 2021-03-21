# ------------------------------------------------------------------------------
# TUNING THE GRAPH LEARNER
# ------------------------------------------------------------------------------

# PURPOSE: tune graph learner to given hyperparameters

# PERFORM TUNING ---------------------------------------------------------------

tune_graph_learner <- function(graph_learner,
                               task,
                               outer_resampling,
                               outer_loss = mlr3::msr("classif.ce"),
                               inner_resampling,
                               inner_loss = mlr3::msr("classif.ce"),
                               hyperparameter_ranges,
                               tuning_iterations) {
  
  # Break early if no tuning is desired
  
  if (any(is.na(hyperparameter_ranges))) return(NA)
  
  set.seed(1L)
  
  # Define hyperparameter search space
  
  hyperparameter_set <- get_hyperparameter_set(
    graph_learner,
    hyperparameter_ranges)
  
  # Terminate tuning after fixed number of iterations
  
  terminator <- trm("evals", n_evals = tuning_iterations)
  
  # Set tuning algorithm to grid search
  
  tuner <- tnr("grid_search", resolution = 1L)
  
  # Create autotuner object
  
  auto_tuner <- AutoTuner$new(
    learner = graph_learner, 
    resampling = inner_resampling, 
    measure = inner_loss,
    search_space = hyperparameter_set, 
    terminator = terminator, 
    tuner = tuner)
  
  # Wrap in outer resampling loop
  
  resample(
    task = task,
    learner = auto_tuner,
    resampling = outer_resampling,
    store_models = TRUE)
  
}

# SET UP HYPERPARAMETER SEARCH SPACE -------------------------------------------

get_hyperparameter_set <- function(graph_learner, hyperparameter_ranges) {
  
  # Get tunable hyperparameters of specified learner 
  
  learner <- graph_learner$graph$ids()[
    which(stringr::str_detect(graph_learner$graph$ids(), "^classif."))]
  
  hp_set <- as.data.table(lrn(learner)$param_set)
  
  # Create paradox parameter objects for parameter set
  
  params_list <- list()
  
  for (hp in seq_along(hyperparameter_ranges)) {
    
    # this_id <- names(hyperparameter_ranges)[hp]
    # this_value <- hyperparameter_ranges[[this_id]]
    
    this_id <- hyperparameter_ranges[[hp]]
    this_range <- hyperparameter_ranges[[hp]]$ranges
    
    # Check whether specified hyperparameters exist
    
    if (!(this_id %in% hp_set[, id])) {
      stop(sprintf("%s is not a hyperparameter of %s", this_id, learner))
    }
    
    # Find correct parameter class
    
    this_hp_set <- hp_set[id == this_id]
    this_hp_class <- switch(
      this_hp_set$storage_type,
      character = "Fct",
      numeric = "Dbl",
      integer = "Int",
      logical = "Lgl",
      list = "Uty")
    
    # As opposed to other parameter classes, for factor parameters the levels
    # are specified as a single character vector parameter (e.g., for double,
    # lower and upper are separate arguments so do.call can handle them as a
    # list)
    
    # TODO Check whether this works for logical and utility parameters also 

    if (this_hp_class == "Fct") this_range <- list(unlist(this_range))
    
    # This looks so ugly because do.call calls a method here, not a function
    # (and as such on an object that does not yet exist)
    
    param <- do.call(
      eval(parse(text = paste0("Param", this_hp_class, "$new"))),
      append(
        paste0(learner, ".", this_id),
        this_range))
    
    params_list[[this_id]] <- param
    
  }
  
  # Create paradox parameter set
  
  param_set <- ps()
  
  for (hp in seq_along(hyperparameter_ranges)) param_set$add(params_list[[hp]])
  
  param_set
  
}

