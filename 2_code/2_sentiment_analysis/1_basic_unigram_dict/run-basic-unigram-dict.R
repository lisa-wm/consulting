# ------------------------------------------------------------------------------
# SENTIMENT ANALYSIS: BASIC APPROACH USING UNIGRAMS & GLOBAL DICTIONARY
# ------------------------------------------------------------------------------

# Purpose: perform SA using the most basic approach (unigrams, bag-of-words
# assumption, global existing dictionary)

# Steps:
# 1. Pre-process data
# 2. Create document-feature matrix
# 3. Create dictionary
# 4. Classify sentiments

# PREREQUISITES ----------------------------------------------------------------

# Install, if necessary, and load required packages

packages_required <-  c(
  "here",
  "tidyverse",
  "data.table",
  "quanteda"
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

files_required <- list(
  here("2_code/1_preprocessing", "fun-preprocess-tweets.R")
)
invisible(sapply(files_required, source, .GlobalEnv))

# STEP 1: PRE-PROCESS DATA -----------------------------------------------------

data <- fread(
  here("1_scraping/output", "tweepy_df_subset.csv"), 
  encoding = "UTF-8")

data_processed <- preprocess_tweets(data)

# STEP 2: CREATE DOCUMENT-FEATURE MATRIX ---------------------------------------

dfm_tweets <- create_dfm(data_processed)

# STEP 3: CREATE DICTIONARY ----------------------------------------------------

global_dictionary <- create_dict()

# STEP 4: CLASSIFY SENTIMENTS --------------------------------------------------

sentiments_basic_dict <- get_sentiments_basic_dict(
  dfm_tweets,
  global_dictionary
)
