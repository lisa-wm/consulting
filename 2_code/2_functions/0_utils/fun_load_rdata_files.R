# ------------------------------------------------------------------------------
# SAVING RDATA FILES
# ------------------------------------------------------------------------------

# PURPOSE: save files created to rdata with automatic file name creation

load_rdata_files <- function(robject, folder, tmp = FALSE) {
  
  robject_name <- deparse(substitute(robject))
  
  # tmp option is used to add a "tmp_" prefix that will be ignored by git
  # Sensible if file should be stored within a running session to build upon it
  # in later steps but permanent storage is not necessary
  
  if (tmp) {
    
    do.call(
      load,
      list(
        file = here(
          folder, 
          sprintf("tmp_rdata_%s.RData", robject_name)),
        envir = .GlobalEnv))
    
  } else {
    
    do.call(
      load,
      list(
        file = here(
          folder, 
          sprintf("rdata_%s.RData", robject_name)),
        envir = .GlobalEnv))
    
  }
 
}
