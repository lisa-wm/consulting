# ------------------------------------------------------------------------------
# EXTRACTION OF ADDITIONAL, TWITTER-SPECIFIC FEATURES
# ------------------------------------------------------------------------------

# IN: data with topic labels
# OUT: data with twitter-specific features

# EXTRACT TWITTER-SPECIFIC FEATURES --------------------------------------------

load_rdata_files(tweets_corpus, folder = "2_code/1_data/2_tmp_data")

tweets_features_response <- convert_qtda_to_dt(
  tweets_corpus,
  key = "doc_id")

tweets_features_response <- tweets_features_response[
  , .(doc_id, hashtags, tags)
  ][, `:=` (
    n_hashtags = lengths(hashtags),
    n_tags = lengths(tags)),
    by = doc_id
    ][, `:=` (hashtags = NULL, tags = NULL)]

save_rdata_files(tweets_features_response, folder = "2_code/1_data/2_tmp_data")

# TODO find sth to do w/ hashtags
