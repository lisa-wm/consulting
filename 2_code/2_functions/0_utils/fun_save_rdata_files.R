# ------------------------------------------------------------------------------
# SAVING RDATA FILES
# ------------------------------------------------------------------------------

# PURPOSE: save files created to rdata with automatic file name creation

save_rdata_files <- function(robject, folder, tmp = TRUE) {

  # Assign to correctly named object in execution environment (otherwise, the 
  # name of the saved object will be "robject", which is also its name after 
  # loading the .RData file)
  
  assign(deparse(substitute(robject)), robject)
  env_vars <- ls()
  
  # Use do.call because save() evaluates its arguments in a weird way, such that
  # neither strings nor get() are usable to reference the object
  
  # tmp option is used to add a "tmp_" prefix that will be ignored by git
  # Sensible if file should be stored within a running session to build upon it
  # in later steps but permanent storage is not necessary
  
  if (tmp) {
    
    do.call(
      save, 
      list(
        env_vars[which(env_vars == deparse(substitute(robject)))], 
        file = here(
          folder, 
          sprintf("tmp_rdata_%s.RData", deparse(substitute(robject))))))
    
  } else {
    
    do.call(
      save, 
      list(
        env_vars[which(env_vars == deparse(substitute(robject)))], 
        file = here(
          folder, 
          sprintf("rdata_%s.RData", deparse(substitute(robject))))))
    
  }
  
}
