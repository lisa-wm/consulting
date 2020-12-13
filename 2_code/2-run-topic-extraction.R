# ------------------------------------------------------------------------------
# TOPIC EXTRACTION
# ------------------------------------------------------------------------------

# Purpose: assign topics to tweets

# Steps:

# 1. Create dfm from per-user documents
# 2. Define topical prevalence variables
# 3. 

# STEP 1: CREATE DFM FROM PER-USER DOCUMENTS -----------------------------------

load(here("2_code", "rdata-tweets-processed.RData"))

# Group on per-user, per-month level

data_processed_user_monthly <- copy(data_processed)[
  , .(username, created_at, full_text)
  ][, `:=` (created_year = year(created_at), created_month = month(created_at))]

tweets_concatenated <- data_processed_user_monthly[
  , paste(full_text, collapse = " "), 
  by = .(username, created_year, created_month)]
setnames(tweets_concatenated, "V1", "full_text_user_month")

data_processed_user_monthly <- unique(data_processed_user_monthly[
  , .(username, created_year, created_month)
  ])[tweets_concatenated, on = list(username, created_year, created_month)
     ][, doc_id := .I]

# Create corpus object

tweets_corpus_user_monthly <- quanteda::corpus(
  x = data_processed_user_monthly,
  docid_field = "doc_id",
  text_field = "full_text_user_month")

# Tokenize corpus with custom stopwords

tweets_tokens_user_monthly <- make_tokens(
  corpus = tweets_corpus_user_monthly, 
  stopwords = make_stopwords())

# Create dfm

tweets_dfm_user_monthly <- make_dfm(
  tokens_ngrams = tweets_tokens_user_monthly,
  min_termfreq = 1L
)
