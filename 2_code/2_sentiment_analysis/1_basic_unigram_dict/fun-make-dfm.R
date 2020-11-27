# ------------------------------------------------------------------------------
# CREATION OF A DOCUMENT-FEATURE MATRIX
# ------------------------------------------------------------------------------

# Purpose: create a document-feature matrix for sentiment analysis

# HELPER FUNCTIONS -------------------------------------------------------------



# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

make_dfm <- function(tokens) {
  
  dfm(
    tokens, 
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
