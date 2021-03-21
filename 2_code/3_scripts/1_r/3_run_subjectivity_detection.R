# ------------------------------------------------------------------------------
# SUBJECTIVITY DETECTION
# ------------------------------------------------------------------------------

# IN: corpus object of cleaned tweets, meta data and static features
# OUT: data filtered for subjective tweets

# FILTER SUBJECTIVE TWEETS -----------------------------------------------------

# TODO make subjectivity detection better if possible

load_rdata_files(tweets_corpus, folder = "2_code/1_data/2_tmp_data")

tweets_subjective <- convert_qtda_to_dt(tweets_corpus, key = "doc_id")

cols_to_keep <- c(
  "doc_id",
  "label",
  "text",
  names(tweets_subjective)[startsWith(names(tweets_subjective), "feat")],
  "meta_party",
  "meta_bundesland",
  "meta_unemployment_rate",
  "twitter_username",
  "twitter_year",
  "twitter_month")

char_cols <- sprintf("feat_%s", letters)

tweets_subjective <- tweets_subjective[, ..cols_to_keep][
  label != "none" | 
    feat_n_emojis > 0 | feat_polarity_positive_strong > 1 | 
    feat_polarity_negative_strong > 1 | feat_favorite_count > 10L
  ][, aux := sum(.SD), .SDcols = char_cols, by = "doc_id"
    ][aux > 0L
      ][, aux := NULL]

data.table::set(
  tweets_subjective, 
  j = "label", 
  value = as.factor(tweets_subjective$label))

save_rdata_files(tweets_subjective, folder = "2_code/1_data/2_tmp_data")
