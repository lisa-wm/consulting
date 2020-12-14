# ------------------------------------------------------------------------------
# CREATION OF A DOCUMENT-FEATURE MATRIX
# ------------------------------------------------------------------------------

# Purpose: create a document-feature matrix for sentiment analysis

# HELPER FUNCTIONS -------------------------------------------------------------

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

make_dfm <- function(tokens_ngrams, min_termfreq = 1, tfidf = TRUE) {
  
  this_dfm <- quanteda::dfm(
    tokens_ngrams, 
    stem = TRUE,
    verbose = FALSE) %>% 
    # quanteda::dfm_wordstem(language = "german") %>% 
    quanteda::dfm_trim(min_termfreq = min_termfreq)
  
  if (tfidf) this_dfm <- this_dfm %>% quanteda::dfm_tfidf()
  
  this_dfm

}
