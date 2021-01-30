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

load_rdata_files(
  tweets_features_dict, 
  folder = "2_code/3_sentiment_analysis")
load_rdata_files(
  tweets_features_lexical, 
  folder = "2_code/3_sentiment_analysis")
load_rdata_files(
  tweets_features_embeddings, 
  folder = "2_code/3_sentiment_analysis")
load_rdata_files(
  tweets_features_response, 
  folder = "2_code/3_sentiment_analysis")

tweets_features <- tweets_features_dict[
  tweets_features_lexical, 
  ][tweets_features_embeddings, 
    ][tweets_features_response, ]

data.table::setnames(tweets_features, tolower(names(tweets_features)))

# FILTER SUBJECTIVE TWEETS -----------------------------------------------------

tweets_subjective <- tweets_features[
  n_emojis > 0 | positive_global > 1 | negative_global > 1 
]

tweets_subjective <- tweets_features[
  n_emojis > 0 | positive_global > 0 | negative_global > 0 |
    exclamation_mark_rep > 0 | question_mark_rep > 0 | 
    repeated_char > 0 | repeated_char_seq > 0 | adj > 0 | adv > 0 |
    n_intensifications > 0 | retweet_count > 10L | favorite_count > 10L
]
