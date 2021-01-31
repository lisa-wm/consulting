# ------------------------------------------------------------------------------
# SET-UP
# ------------------------------------------------------------------------------

# Purpose: load required packages

# Install, if necessary, and load required packages

packages_required <-  c(
  "here",
  "tidyverse",
  "data.table",
  "quanteda",
  "cld3",
  "checkmate",
  "testthat",
  "XML",
  "mlr3",
  "mlr3learners"
)

set_up_packages <- function(pkg) {
  
  my_type <- ifelse(
    Sys.info()[["sysname"]] == "Linux", 
    "source", 
    "binary"
  )
  
  not_installed <- 
    packages_required[!packages_required %in% installed.packages()[, "Package"]]
  
  if (length(not_installed) > 0) {
    
    lapply(
      not_installed,
      install.packages,
      repos = "http://cran.us.r-project.org",
      dependencies = TRUE,
      type = my_type
    )
    
  }
  
  lapply(packages_required, library, character.only = TRUE, quietly = TRUE)
  
}

invisible(set_up_packages(packages_required))

# Source required files containing sub-level functions
# FIXME Check sourcing  
# Why does sourcing preprocess-tweets not work, only if functions are
# assigned manually?
# Should source ALL functions

# files_required <- list(
#   here("2_code/1_preprocessing", "fun-get-data.R"),
#   here("2_code/1_preprocessing", "fun-preprocess-tweets.R"),
#   here("2_code/1_preprocessing", "fun-preprocess-meta.R")
# )
# invisible(sapply(files_required, source, .GlobalEnv))
