# ------------------------------------------------------------------------------
# TOKENIZATION OF CORPUS
# ------------------------------------------------------------------------------

# Purpose: create tokens out of basic corpus

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

sentence <- "John Doe is a dumbass whose significance I never got. Full stop."

tokens(sentence, remove_punct = TRUE)

# TODO Set up stemming and lemmatization

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

make_dfm <- function(corpus) {
  
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

dfm_tweets <- make_dfm(tweets_corpus[1:10])
topfeatures(dfm_tweets, 200)
