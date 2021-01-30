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
  n_emojis > 0 | positive_global_strong > 1 | negative_global_strong > 1 | 
    exclamation_mark_rep > 0 | question_mark_rep > 0 | 
    repeated_char > 0 | repeated_char_seq > 0 | favorite_count > 100L
]

tweets_objective <- tweets_features[!(doc_id %in% tweets_subjective$doc_id)]

# REDUCE CORPUS FOR SENTIMENT ANALYSIS TO SUBJECTIVE DOCUMENTS -----------------

load_rdata_files(tweets_corpus_topics_unsupervised, folder = "2_code")

tweets_corpus_sa <- convert_qtda_to_dt(
  tweets_corpus_topics_unsupervised, 
  key = "doc_id"
)

tweets_corpus_sa <- tweets_corpus_sa[
  , .(doc_id, topic_label, retweet_count, favorite_count, word_count)
  ][tweets_features, ]

tweets_corpus_sa <- quanteda::corpus_subset(
  tweets_corpus_topics_unsupervised)
