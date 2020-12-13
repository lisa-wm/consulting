# ------------------------------------------------------------------------------
# SENTIMENT ANALYSIS: BASIC APPROACH USING DICTIONARIES
# ------------------------------------------------------------------------------

# Purpose: perform SA using dictionaries

# Steps:

# 1. Create dictionaries
# 2. Classify sentiments

# STEP 1: CREATE DICTIONARIES --------------------------------------------------

# Make unigram dictionary (might take a couple of minutes)

global_dict_unigrams <- make_dict_unigram(
  source_positive = here(
    "2_code/2_sentiment_analysis/1_dict_based/dicts", 
    "GermanPolarityClues-Positive-21042012.tsv"),
  source_negative = here(
    "2_code/2_sentiment_analysis/1_dict_based/dicts", 
    "GermanPolarityClues-Negative-21042012.tsv")
)

save(
  global_dict_unigrams,
  file = here(
    "2_code/2_sentiment_analysis/1_dict_based",
    "rdata-global-dict-unigrams.RData")
)

# load(here(
#   "2_code/2_sentiment_analysis/1_dict_based",
#   "rdata-global-dict-unigrams.RData"))

# STEP 2: CLASSIFY SENTIMENTS --------------------------------------------------

# Get sentiments based on unigrams

load(here("2_code", "rdata-tweets-processed.RData"))
load(here("2_code/1_preprocessing", "rdata-tweets-dfm-unigrams.RData"))

sentiments_dict_unigrams <- make_sentiments_dict(
  tweets_dfm_unigrams,
  global_dict_unigrams
)

# Append labels to data and save

if (nrow(data_processed) != nrow(sentiments_dict_unigrams)) {
  stop("some rows appear to be missing, join will cause loss of data")
}

data_labeled_dict_unigrams <- data_processed[
  sentiments_dict_unigrams, on = "doc_id"]

save(
  data_labeled_dict_unigrams,
  file = here("2_code", "rdata-tweets-labeled-dict-unigrams.RData"))
