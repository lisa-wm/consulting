# ------------------------------------------------------------------------------
# SENTIMENT ANALYSIS: BASIC APPROACH USING UNIGRAMS & GLOBAL DICTIONARY
# ------------------------------------------------------------------------------

# Purpose: perform SA using the most basic approach (unigrams, bag-of-words
# assumption, global existing dictionary)

# Steps:
# 1. Pre-process data (primitively)
# 2. Create document-feature matrix
# 3. Create dictionary
# 4. Classify sentiments

# PREREQUISITES ----------------------------------------------------------------

# Install, if necessary, and load required packages

packages_required <-  c(
  "here",
  "tidyverse",
  "data.table",
  "quanteda",
  "cld3",
  "checkmate",
  "testthat",
  "XML"
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

files_required <- list(
  here("2_code/1_preprocessing", "fun-get-data.R"),
  here("2_code/1_preprocessing", "fun-preprocess-tweets.R"),
  here("2_code/1_preprocessing", "fun-preprocess-meta.R")
)
invisible(sapply(files_required, source, .GlobalEnv))

# STEP 1: PRE-PROCESS DATA (PRIMITIVELY) ---------------------------------------

# Read data and split into text and meta data

tweepy_df_subset <- 
  get_data(path = here("1_scraping/output", "tweepy_df_subset.csv"))

tweets_raw <- tweepy_df_subset %>% 
  select(doc_id, full_text)

tweets_metadata <- tweepy_df_subset %>% 
  select(-full_text)

# Process tweets such that NLP analyses can be carried out

tweets_processed <- tweets_raw %>% 
  preprocess_basic() %>% 
  mutate(word_count = ntoken(full_text, remove_punct = TRUE))

# Process metadata

tweets_metadata_processed <- tweets_metadata %>% 
  preprocess_meta()

# Save for further analysis

tweepy_df_subset_processed <- left_join(
  tweets_metadata_processed, 
  tweets_processed, 
  by = "doc_id"
)

save(
  tweepy_df_subset_processed, 
  file = here("2_code", "tweepy_df_subset_processed.RData"))

# STEP 2: CREATE DOCUMENT-FEATURE MATRIX ---------------------------------------

# Create quanteda corpus

tweets_corpus <- corpus(
  tweets_processed,
  docid_field = "doc_id",
  text_field = "full_text"
)

# Create dfm out of processed tweets

dfm_tweets <- create_dfm(tweets_corpus)

# STEP 3: CREATE DICTIONARY ----------------------------------------------------

# global_dictionary <- create_dict()

# STEP 4: CLASSIFY SENTIMENTS --------------------------------------------------

# sentiments_basic_dict <- get_sentiments_basic_dict(
#   dfm_tweets,
#   global_dictionary
# )
