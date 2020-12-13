# ------------------------------------------------------------------------------
# PRE-PROCESSING TWEETS
# ------------------------------------------------------------------------------

# Purpose: prepare tweets for SA by creating document-feature-matrices

# Steps:
# 1. Perform basic text cleaning
# 2. Perform lemmatization
# 2. Create document-feature-matrix using mlr3's text pipeop

# STEP 1: PERFORM BASIC TEXT CLEANING ------------------------------------------

# Read data (if retweets are still included, set option to TRUE)

data_raw <- get_data(
  path = here("1_scraping/output", "tweepy_df_subset_no_retweets.csv"),
  is_old_version = FALSE)

# Process tweets in a very basic way - remove umlauts, symbols etc. but keep
# text original otherwise (feature extraction is carried out in step 2);
# convert list and date columns into suitable formats

data_processed <- data_raw %>% 
  make_clean_tweets(column = "full_text") %>% 
  make_clean_meta(
    list_columns = list("hashtags", "mentions"), 
    date_columns = list("created_at"))

data_processed[, word_count := quanteda::ntoken(full_text, remove_punct = TRUE)]

# TODO Remove resigned MP

# Save for further analysis

save(
  data_processed, 
  file = here("2_code", "rdata-tweets-processed.RData")
)

# load(here("2_code", "rdata-tweets-processed.RData"))

# STEP 2: PERFORM LEMMATIZATION ------------------------------------------------

# TODO Implement lemmatization (if necessary)

# STEP 3: CREATE DOCUMENT-FEATURE-MATRIX ---------------------------------------

# First, create corpus object

tweets_corpus <- quanteda::corpus(
  x = data_processed,
  docid_field = "doc_id",
  text_field = "full_text")

# Tokenize corpus with custom stopwords

tweets_tokens <- make_tokens(
  corpus = tweets_corpus, 
  stopwords = make_stopwords())

# If desired, create n-grams (n = 1L this returns the original tokens)
# The skip argument can be used to create n-grams from tokens that are not
# immediate neighbors but further apart, which could be useful for German
# (e.g., for a phrase like "Ich mag das nicht" it could be desirable to have a 
# token "mag_nicht").

tweets_unigrams <- quanteda::tokens_ngrams(tweets_tokens, n = 1L)
tweets_bigrams <- quanteda::tokens_ngrams(tweets_tokens, n = 2L)

# Create dfm

tweets_dfm_unigrams <- make_dfm(
  tokens_ngrams = tweets_unigrams,
  min_termfreq = 1L
)

save(
  tweets_dfm_unigrams,
  file = here("2_code/1_preprocessing", "rdata-tweets-dfm-unigrams.RData")
)

