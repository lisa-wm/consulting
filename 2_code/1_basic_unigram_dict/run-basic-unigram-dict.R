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
  here("1_scraping/scripts_r", "fun-set-up-selenium.R"),
  here("1_scraping/scripts_r", "fun-get-mp-metadata.R")
)
invisible(sapply(files_required, source, .GlobalEnv))

# STEP 1: PRE-PROCESS DATA -----------------------------------------------------

my_text <- c("Alice loves ice cream",
             "Bob hates pickles",
             "Colin is furious")
my_tokens <- tokens(my_text, remove_punct = TRUE)

# STEP 2: CREATE DOCUMENT-FEATURE MATRIX ---------------------------------------

df_mat <- dfm(my_tokens)

cooccurrence_mat <- fcm(my_tokens)
matrix(cooccurrence_mat, ncol = sqrt(length(cooccurrence_mat)))

# STEP 3: CREATE DICTIONARY ----------------------------------------------------

data(sentiments, package = "tidytext")
my_dict <- as.dictionary(sentiments)

# STEP 4: CLASSIFY SENTIMENTS --------------------------------------------------

dfm_lookup(df_mat, my_dict)
