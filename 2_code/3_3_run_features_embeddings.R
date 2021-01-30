# ------------------------------------------------------------------------------
# EXTRACTION OF WORD EMBEDDINGS
# ------------------------------------------------------------------------------

# IN: data with topic labels
# OUT: data with loadings on word embeddings

load_rdata_files(tweets_corpus_topics_unsupervised, folder = "2_code")

tweets_embeddings <- convert_qtda_to_dt(
  tweets_corpus_topics_unsupervised,
  key = "doc_id")

tweets_features_embeddings <- tweets_embeddings[
  , .(doc_id)
  ][, foo := "wazzup"]

save_rdata_files(
  tweets_features_embeddings, 
  folder = "2_code/3_sentiment_analysis")
