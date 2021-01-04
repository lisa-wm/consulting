# ------------------------------------------------------------------------------
# PRE-PROCESSING TWEETS
# ------------------------------------------------------------------------------

# Purpose: prepare tweets for SA by creating document-feature-matrices

# Steps:
# 1. Perform basic text cleaning
# 2. Append socio-electoral data (used for topic extraction)
# 2. Create document-feature-matrix using mlr3's text pipeop

# STEP 1: PERFORM BASIC TEXT CLEANING ------------------------------------------

# Read data (if retweets are still included, set option to TRUE)

tweets_raw <- get_data(
  path = here("1_scraping/output", "tweepy_df_subset_no_retweets.csv"),
  is_old_version = FALSE)

# Process tweets in a very basic way - remove umlauts, symbols etc. but keep
# text original otherwise (feature extraction is carried out in step 2);
# convert list and date columns into suitable formats

tweets_processed <- tweets_raw %>% 
  make_clean_tweets(column = "full_text") %>% 
  make_clean_meta(
    list_columns = list("hashtags", "mentions"), 
    date_columns = list("created_at"))

# Remove data created prior to 2017-09-24, the date of the federal election

tweets_processed <- tweets_processed[created_at >= "2017-09-24"]

# Measure length of tweets as number of words

tweets_processed[
  , word_count := quanteda::ntoken(full_text, remove_punct = TRUE)]

# TODO Remove resigned MP

# Save for further analysis

save(
  tweets_processed, 
  file = here("2_code", "rdata-tweets-processed.RData")
)

# load(here("2_code", "rdata-tweets-processed.RData"))

# TODO Implement lemmatization (if necessary)

# STEP 2: APPEND SOCIO-ELECTORAL DATA ------------------------------------------

data_mp_level <- data.table::fread(
  here("1_scraping/output", "abg_twitter_df.csv"),
  encoding = "UTF-8",
  sep = ",",
  drop = "twitter")

data_socio_electoral <- data.table::fread(
  here("1_scraping/output", "socioeconomics_zweitstimmen_df.csv"),
  encoding = "UTF-8",
  sep = ",",
  drop = c("district", "wahlkreis")
)

# This takes a few seconds

tweets_processed_with_covariates <- append_covariates(
  tweets_data = tweets_processed,
  mp_data = data_mp_level,
  se_data = data_socio_electoral
)

# STEP 3: CREATE DOCUMENT-FEATURE-MATRIX ---------------------------------------

# First, create corpus object

tweets_corpus <- quanteda::corpus(
  x = tweets_processed_with_covariates,
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

# Create dfm (unweighted version for topic extraction)

tweets_dfm_unigrams <- make_dfm(
  tokens_ngrams = tweets_unigrams,
  min_termfreq = 10L
)

save(
  tweets_dfm_unigrams,
  file = here("2_code/1_preprocessing", "rdata-tweets-dfm-unigrams.RData")
)

# Create weighted version for sentiment analysis

tweets_dfm_unigrams_tfidf <- tweets_dfm_unigrams %>% 
  quanteda::dfm_tfidf()

save(
  tweets_dfm_unigrams_tfidf,
  file = here("2_code/1_preprocessing", "rdata-tweets-dfm-unigrams-tfidf.RData")
)

