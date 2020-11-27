# ------------------------------------------------------------------------------
# CREATION OF A DOCUMENT-FEATURE MATRIX
# ------------------------------------------------------------------------------

# Purpose: create a document-feature matrix for sentiment analysis

# HELPER FUNCTIONS -------------------------------------------------------------

# TODO Make good stopword list

get_stopwords <- function() {
  
  remove_umlauts(stopwords("de"))
  
  file_stopwords <- here(
    "2_code/2_sentiment_analysis/1_basic_unigram_dict/dicts", 
    "german_stopwords.xml")
  german_stopwords <- 
    xmlToDataFrame(xmlParse(file_stopwords, encoding = "UTF-8"))
  
  remove_umlauts(german_stopwords$text)
  
}

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

create_dfm <- function(corpus) {
  
  dfm(
    corpus, 
    remove = c(
      get_stopwords(),
      "@*"), 
    remove_punct = TRUE, 
    remove_numbers = TRUE,
    stem = TRUE,
    verbose = FALSE)
  
}

dfm_tweets <- create_dfm(tweets_corpus[1:10])
topfeatures(dfm_tweets, 200)
