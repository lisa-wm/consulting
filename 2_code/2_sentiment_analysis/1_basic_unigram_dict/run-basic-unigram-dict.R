# ------------------------------------------------------------------------------
# SENTIMENT ANALYSIS: BASIC APPROACH USING UNIGRAMS & GLOBAL DICTIONARY
# ------------------------------------------------------------------------------

# Purpose: perform SA using the most basic approach (unigrams, bag-of-words
# assumption, global existing dictionary)

# Steps:
# 1. Pre-process data (primitively)
# 2. Create corpus
# 3. Create tokens
# 4. Create document-feature matrix
# 5. Create dictionary
# 6. Classify sentiments

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

# Process tweets in a very basic way - remove umlauts, symbols etc. but keep
# text original otherwise
# Feature extraction is carried out in step 2

tweets_processed <- tweepy_df_subset %>% 
  select(doc_id, full_text) %>% 
  preprocess_basic() %>% 
  mutate(word_count = quanteda::ntoken(full_text, remove_punct = TRUE))

# Process metadata

tweets_metadata_processed <- tweepy_df_subset %>% 
  select(-full_text) %>% 
  preprocess_meta()

# Save for further analysis

tweepy_df_subset_processed <- left_join(
  tweets_metadata_processed, 
  tweets_processed, 
  by = "doc_id"
)

save(
  tweepy_df_subset_processed, 
  file = here("2_code", "tweepy_df_subset_processed.RData")
)

# STEP 2: CREATE CORPUS --------------------------------------------------------

# Create quanteda corpus (non-text columns are preserved and can be accessed
# via docvars())

# load(here("2_code", "tweepy_df_subset_processed.RData"))

tweets_corpus <- quanteda::corpus(
  tweepy_df_subset_processed,
  docid_field = "doc_id",
  text_field = "full_text"
)

# STEP 3: CREATE TOKENS --------------------------------------------------------

stopwords_custom <- get_stopwords()
save(
  stopwords_custom, 
  file = here(
    "2_code/2_sentiment_analysis/1_basic_unigram_dict/dicts",
    "stopwords_custom.RData"))

tweets_tokens <- make_tokens(tweets_corpus, stopwords_custom)

# STEP 4: CREATE DOCUMENT-FEATURE MATRIX ---------------------------------------

# Create dfm out of processed tweets

tweets_dfm <- make_dfm(
  tweets_tokens,
  min_termfreq = 10,
  stemming = FALSE,
  tfidf = TRUE)

topfeatures(tweets_dfm, 100)

# STEP 5: CREATE DICTIONARY ----------------------------------------------------

global_unigram_dictionary <- create_unigram_dict(
  source_positive = here(
    "2_code/2_sentiment_analysis/1_basic_unigram_dict/dicts", 
    "GermanPolarityClues-Positive-21042012.tsv"),
  source_negative = here(
    "2_code/2_sentiment_analysis/1_basic_unigram_dict/dicts", 
    "GermanPolarityClues-Negative-21042012.tsv")
)

save(
  global_unigram_dictionary,
  file = here(
    "2_code/2_sentiment_analysis/1_basic_unigram_dict/dicts",
    "global_unigram_dictionary.RData")
)

# STEP 6: CLASSIFY SENTIMENTS --------------------------------------------------

# Get sentiments based on unigrams dictionary

sentiments_basic_unigram_dict <- get_sentiments_basic_unigram_dict(
  tweets_dfm,
  global_unigram_dictionary
)

# Determine confidence via difference in detected positive and negative labels,
# weighted by number of found sentiments

median_sentiments_found <- 
  median(sentiments_basic_unigram_dict$sentiments_found)

sentiments_basic_unigram_dict <- sentiments_basic_unigram_dict %>% 
  mutate(
    doc_id = as.numeric(doc_id),
    confidence = sentiments_found / median_sentiments_found * abs(diff_pos)
  )

table(sentiments_basic_unigram_dict$label)

# Append labels to data and save

tweepy_df_subset_labeled <- tweepy_df_subset_processed %>% 
  left_join(sentiments_basic_unigram_dict, by = "doc_id")

save(
  tweepy_df_subset_labeled,
  file = here("2_code", "tweepy_df_subset_labeled.RData"))
