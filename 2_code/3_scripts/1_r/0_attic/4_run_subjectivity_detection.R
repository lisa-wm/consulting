# ------------------------------------------------------------------------------
# SUBJECTIVITY DETECTION
# ------------------------------------------------------------------------------

# IN: data with topic labels and various features
# OUT: data bearing sentiment

# ideas. keep sentences that contain adjectives, polarity clues, emojis, maybe
# personal pronouns like "ich"
# look for subjectivity clues dict
# discard tweets w/ numbers?

# COLLECT DATA FEATURES --------------------------------------------------------

load_rdata_files(tweets_features_dict, folder = "2_code/1_data/2_tmp_data")
load_rdata_files(tweets_features_lexical, folder = "2_code/1_data/2_tmp_data")
# load_rdata_files(tweets_features_embed, folder = "2_code/1_data/2_tmp_data")
load_rdata_files(tweets_features_response, folder = "2_code/1_data/2_tmp_data")

tweets_features <- tweets_features_dict[
  tweets_features_lexical, 
  # ][tweets_features_embed, 
    ][tweets_features_response, ]

load_rdata_files(tweets_corpus, folder = "2_code/1_data/2_tmp_data")

tweets_sa <- convert_qtda_to_dt(tweets_corpus, key = "doc_id")

tweets_sa <- tweets_sa[
  , .(doc_id, 
      text, 
      topic_label_stm, 
      retweet_count, 
      favorite_count, 
      word_count,
      label)
  ][tweets_features, ]

# FILTER SUBJECTIVE TWEETS -----------------------------------------------------

tweets_sa <- tweets_sa[
  label != "none" | 
    n_emojis > 0 | positive_global_strong > 1 | negative_global_strong > 1 |
    exclamation_mark_rep > 0 | question_mark_rep > 0 |
    repeated_char > 0 | repeated_char_seq > 0 | favorite_count > 100L]

save_rdata_files(tweets_sa, folder = "2_code/1_data/2_tmp_data")
