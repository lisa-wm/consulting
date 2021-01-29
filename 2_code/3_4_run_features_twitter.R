# ------------------------------------------------------------------------------
# EXTRACTION OF ADDITIONAL, TWITTER-SPECIFIC FEATURES
# ------------------------------------------------------------------------------

# IN: data with topic labels
# OUT: data with twitter-specific features

# EXTRACT TWITTER-SPECIFIC FEATURES --------------------------------------------

load_rdata_files(tweets_corpus, folder = "2_code")

tweets_response <- convert_dfm_to_dt(tweets_corpus, key = "doc_id")

tweets_response <- tweets_response[
  , .(doc_id,
      retweet_count,
      favorite_count,
      word_count,
      hashtags,
      tags)
  ][, `:=` (
    n_hashtags = lengths(hashtags),
    n_tags = lengths(tags)),
    by = doc_id]

# TODO find sth to do w/ hashtags
