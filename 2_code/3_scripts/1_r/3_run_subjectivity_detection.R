# ------------------------------------------------------------------------------
# SUBJECTIVITY DETECTION
# ------------------------------------------------------------------------------

# IN: corpus object of cleaned tweets, meta data and static features
# OUT: data filtered for subjective tweets

# FILTER SUBJECTIVE TWEETS -----------------------------------------------------

# TODO make subjectivity detection better if possible

load_rdata_files(tweets_corpus, folder = "2_code/1_data/2_tmp_data")

tweets_subjective <- convert_qtda_to_dt(tweets_corpus, key = "doc_id")[
  label != "none" | 
    n_emojis > 0 | positive_global_strong > 1 | negative_global_strong > 1 |
    exclamation_mark_rep > 0 | question_mark_rep > 0 |
    repeated_char > 0 | repeated_char_seq > 0 | favorite_count > 10L
  ][, c("emojis", "hashtags", "tags") := NULL
    ][, doc_id := NULL]

save_rdata_files(tweets_subjective, folder = "2_code/1_data/2_tmp_data")
