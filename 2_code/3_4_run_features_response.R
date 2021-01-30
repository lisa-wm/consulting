# ------------------------------------------------------------------------------
# EXTRACTION OF ADDITIONAL, TWITTER-SPECIFIC FEATURES
# ------------------------------------------------------------------------------

# IN: data with topic labels
# OUT: data with twitter-specific features

# EXTRACT TWITTER-SPECIFIC FEATURES --------------------------------------------

load_rdata_files(tweets_corpus_topics_unsupervised, folder = "2_code")

tweets_response <- convert_qtda_to_dt(
  tweets_corpus_topics_unsupervised,
  key = "doc_id")

tweets_features_response <- tweets_response[
  , .(doc_id, hashtags, tags)
  ][, `:=` (
    n_hashtags = lengths(hashtags),
    n_tags = lengths(tags)),
    by = doc_id
    ][, `:=` (hashtags = NULL, tags = NULL)]

save_rdata_files(
  tweets_features_response, 
  folder = "2_code/3_sentiment_analysis")

# TODO find sth to do w/ hashtags
