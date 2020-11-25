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
  "quanteda",
  "cld3",
  "checkmate",
  "testthat"
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
  here("2_code/1_preprocessing", "fun-preprocess-tweets.R"),
  here("2_code/1_preprocessing", "fun-preprocess-meta.R"),
  here("2_code/2_sentiment_analysis/1_basic_unigram_dict", "fun-create-dfm.R")
)
invisible(sapply(files_required, source, .GlobalEnv))

# STEP 1: PRE-PROCESS DATA -----------------------------------------------------

# Read data

data <- fread(
  here("1_scraping/output", "tweepy_df_subset.csv"), 
  encoding = "UTF-8",
  sep = ",",
  drop = "quoted_status")

# Substitute "full_text" by "retweet_full_text" for retweets
# !!!!!!!! fix that in scraping process, we don't need both columns !!!!!!!!!!!!

data <- data %>% 
  mutate(full_text = ifelse(is_retweet == 1, retweet_full_text, full_text)) %>% 
  select(-retweet_full_text)

# Keep only tweets in German language
# !!!!!!! Language detection is not perfect, maybe fine-tune this !!!!!!!!!!!!!!
# But seems to have trouble with short tweets primarily - not too well suited
# for SA either, presumably

data <- data %>% 
  filter(detect_language(full_text) == "de")

# Split data into metadata and tweet texts, assigning each tweet a unique ID

data <- data %>% 
  mutate(doc_id = row_number())

tweets_raw <- data %>% 
  select(doc_id, full_text)

tweets_metadata <- data %>% 
  select(-full_text)

# Process tweets such that NLP analyses can be carried out

tweets_processed_intermediary <- preprocess_basic(tweets_raw)
tweets_processed <- preprocess_advanced(tweets_processed_intermediary)

# Process metadata

tweets_metadata_processed <- preprocess_meta(tweets_metadata)

# Save for further analysis

tweepy_df_subset_processed <- left_join(
  tweets_metadata_processed, 
  tweets_processed, 
  by = "doc_id"
)
save(
  tweepy_df_subset_processed, 
  file = here("2_code", "tweepy_df_subset_processed.RData"))

# Create quanteda corpus

tweets_corpus <- corpus(
  tweets_processed,
  docid_field = "doc_id",
  text_field = "full_text"
)

# STEP 2: CREATE DOCUMENT-FEATURE MATRIX ---------------------------------------

# Create corpus out of processed tweets

stop_words <- remove_umlauts(stopwords("de"))
dfm_tweets <- create_dfm(tweets_corpus, stop_words)

topfeatures(dfm_tweets, 200)

# STEP 3: CREATE DICTIONARY ----------------------------------------------------

# global_dictionary <- create_dict()

# STEP 4: CLASSIFY SENTIMENTS --------------------------------------------------

# sentiments_basic_dict <- get_sentiments_basic_dict(
#   dfm_tweets,
#   global_dictionary
# )
