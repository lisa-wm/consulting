# ------------------------------------------------------------------------------
# EXTRACTION OF WORD EMBEDDINGS
# ------------------------------------------------------------------------------

# IN: data with topic labels
# OUT: data with loadings on word embeddings

load_rdata_files(tweets_corpus_topics_unsupervised, folder = "2_code")

load_rdata_files(tweets_corpus_tagged, folder = "2_code/3_sentiment_analysis")

tweets_important_words <- tweets_corpus_tagged[
  pos %in% c("VERB", "NOUN", "ADV", "ADJ", "PROPN")
  ][nchar(token) > 2
    ][, aux := "text"]

tweets_important_words <- data.table::dcast(
  tweets_important_words,
  doc_id ~ aux,
  value.var = "token",
  fun.aggregate = paste, 
  collapse = " ")

stopwords_sa <- make_stopwords_sa()
stopwords_sa <- stopwords_sa[nchar(stopwords_sa) > 2L]

tweets_important_words[
  , text := SnowballC::wordStem(tolower(text), language = "de")
  ][, text := stringr::str_squish(unlist(stringr::str_remove_all(
    text, 
    regex(str_c("\\b", stopwords_sa, "\\b", collapse = '|')))))]

tweets_topic_labels <- data.table(
  doc_id = quanteda::docid(tweets_corpus_topics_unsupervised),
  topic_label = tweets_corpus_topics_unsupervised$topic_label
)

tweets_dense <- tweets_topic_labels[
  tweets_important_words, on = "doc_id"
  ][, aux := "text_topic"]

tweets_dense_topics <- data.table::dcast(
  tweets_dense,
  topic_label ~ aux,
  value.var = "text",
  fun.aggregate = paste, 
  collapse = " ")

nchar(tweets_dense_topics$text_topic) # fuck

save_rdata_files(
  tweets_features_embeddings, 
  folder = "2_code/3_sentiment_analysis")
