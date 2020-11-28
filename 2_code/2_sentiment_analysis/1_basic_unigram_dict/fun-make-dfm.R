# ------------------------------------------------------------------------------
# CREATION OF A DOCUMENT-FEATURE MATRIX
# ------------------------------------------------------------------------------

# Purpose: create a document-feature matrix for sentiment analysis

# HELPER FUNCTIONS -------------------------------------------------------------



# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

make_dfm <- function(tokens, 
                     min_termfreq, 
                     stemming = FALSE,
                     tfidf = TRUE) {
  
  dfmat <- quanteda::dfm(tokens, verbose = FALSE)
  
  if (stemming) dfmat <- dfmat %>% quanteda::dfm_wordstem(language = "german") 
  
  dfmat <- dfmat %>%
    quanteda::dfm_trim(min_termfreq = min_termfreq) 
  
  if (tfidf == TRUE) dfmat <- dfmat %>% quanteda::dfm_tfidf()
  
  dfmat
  
}

