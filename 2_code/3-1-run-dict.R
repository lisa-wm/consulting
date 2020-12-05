# ------------------------------------------------------------------------------
# SENTIMENT ANALYSIS: BASIC APPROACH USING DICTIONARIES
# ------------------------------------------------------------------------------

# Purpose: perform SA using dictionaries

# Steps:

# 1. Create dictionaries
# 2. Classify sentiments

# STEP 1: CREATE DICTIONARIES --------------------------------------------------

# Make unigram dictionary (might take a couple of minutes)

global_dict_unigram <- make_dict_unigram(
  source_positive = here(
    "2_code/2_sentiment_analysis/1_dict_based/dicts", 
    "GermanPolarityClues-Positive-21042012.tsv"),
  source_negative = here(
    "2_code/2_sentiment_analysis/1_dict_based/dicts", 
    "GermanPolarityClues-Negative-21042012.tsv")
)

save(
  global_dict_unigram,
  file = here(
    "2_code/2_sentiment_analysis/1_dict_based/dicts",
    "global_dict_unigram.RData")
)

# load(here(
#   "2_code/2_sentiment_analysis/1_dict_based/dicts",
#   "global_dict_unigram.RData"))

# STEP 2: CLASSIFY SENTIMENTS --------------------------------------------------

# Get sentiments based on unigrams

sentiments_dict_unigram <- get_sentiments_dict_unigram(
  tweets_dfm_unigram,
  global_dict_unigram
)

# Append labels to data and save

if (nrow(data_processed) != nrow(sentiments_dict_unigram)) {
  stop("some rows appear to be missing, join will cause loss of data")
}

data_labeled_dict_unigram <- data_processed[
  sentiments_dict_unigram, on = "doc_id"]

save(
  data_labeled_dict_unigram,
  file = here("2_code", "tweepy_df_subset_labeled.RData"))
