# ------------------------------------------------------------------------------
# SET-UP
# ------------------------------------------------------------------------------

# Purpose: load required packages

# Install, if necessary, and load required packages

packages_required <-  c(
  
  "checkmate", # input checking
  "cld3", # language detection
  "data.table", # data wrangling
  "here", # path management
  "mlr3", # machine learning basics
  "mlr3learners", # machine learning learners
  "mlr3pipelines", # machine learning pipelining
  "mlr3tuning", # machine learning tuning
  "paradox", # handling parameter spaces
  "quanteda", # natural language processing
  "spacyr", # lemmatization
  "stm", # structural topic modeling
  "testthat", # code testing
  "tidyverse", # data wrangling
  "XML" # xml parsing
  
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

files_required <- list.files(
  here("2_code"), 
  pattern = "^fun_.*\\.R$", 
  recursive = TRUE,
  full.names = TRUE)

invisible(sapply(files_required, source, .GlobalEnv))