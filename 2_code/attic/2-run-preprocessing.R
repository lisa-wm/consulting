# ------------------------------------------------------------------------------
# PRE-PROCESSING TWEETS
# ------------------------------------------------------------------------------

# Purpose: prepare tweets for SA by creating document-feature-matrices

# Steps:
# 1. Perform basic text cleaning
# 2. Create corpus
# 3. Create tokens
# 4. Create document-feature matrices

# STEP 1: PERFORM BASIC TEXT CLEANING ------------------------------------------

# Read data (if retweets are still included, set option to TRUE)

data_raw <- get_data(
  path = here("1_scraping/output", "tweepy_df_subset_no_retweets.csv"),
  is_old_version = FALSE)

# Process tweets in a very basic way - remove umlauts, symbols etc. but keep
# text original otherwise (feature extraction is carried out in step 2);
# convert list and date columns into suitable formats

data_processed <- data_raw %>% 
  clean_tweets(column = "full_text") %>% 
  clean_meta(
    list_columns = list("hashtags", "mentions"), 
    date_columns = list("created_at"))

data_processed[, word_count := quanteda::ntoken(full_text, remove_punct = TRUE)]

# Save for further analysis

save(
  data_processed, 
  file = here("2_code", "tweepy_df_subset_processed.RData")
)

# STEP 2: CREATE CORPUS --------------------------------------------------------

# Create quanteda corpus (non-text columns are preserved and can be accessed
# via docvars())

# load(here("2_code", "tweepy_df_subset_processed.RData"))

tweets_corpus <- quanteda::corpus(
  data_processed,
  docid_field = "doc_id",
  text_field = "full_text"
)

# STEP 3: CREATE TOKENS --------------------------------------------------------

# TODO Find good solution for stopwords
# Perhaps consider negations only for bigrams?
# TODO Find solution for negations
# TODO Perform text normalization (i.e., from gooood to good, remove hyphens,
# etc.)

stopwords_custom <- make_stopwords()

save(
  stopwords_custom, 
  file = here("2_code/1_preprocessing", "stopwords_custom.RData"))

# load(here("2_code/1_preprocessing", "stopwords_custom.RData"))

tweets_tokens_unigram <- make_tokens_unigram(tweets_corpus, stopwords_custom)

save(
  tweets_tokens_unigram,
  file = here("2_code/1_preprocessing", "tweets_tokens_unigram.RData"))

# load(here("2_code/1_preprocessing", "tweets_tokens_unigram.RData"))

# STEP 4: CREATE DOCUMENT-FEATURE MATRICES -------------------------------------

# Create dfm out of processed tweets

tweets_dfm_unigram <- make_dfm_unigram(
  tweets_tokens_unigram,
  min_termfreq = 10,
  stemming = FALSE,
  tfidf = TRUE)

save(
  tweets_dfm_unigram,
  file = here("2_code/1_preprocessing", "tweets_dfm_unigram.RData"))

# load(here("2_code/1_preprocessing", "tweets_dfm_unigram.RData"))

topfeatures(tweets_dfm_unigram, 100)