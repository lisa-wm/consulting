# ------------------------------------------------------------------------------
# SAVING RDATA FILES
# ------------------------------------------------------------------------------

# PURPOSE: save files created to rdata with automatic file name creation

load_rdata_files <- function(robject, folder) {
  
  robject_name <- deparse(substitute(robject))
  
  do.call(
    load,
    list(
      file = here(
        folder, 
        paste0("rdata_", robject_name, ".RData")),
      envir = .GlobalEnv))
 
}
