# ------------------------------------------------------------------------------
# CREATION OF A DOCUMENT-FEATURE MATRIX
# ------------------------------------------------------------------------------

# Purpose: create a document-feature matrix for sentiment analysis

# HELPER FUNCTIONS -------------------------------------------------------------



# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

make_dfm <- function(tokens, min_termfreq = 10, stemming = TRUE) {
  
  dfm <- quanteda::dfm(tokens, verbose = FALSE)
  
  if (stemming) dfm <- dfm %>% quanteda::dfm_wordstem(language = "german") 
  
  dfm %>%
    quanteda::dfm_trim(min_termfreq = min_termfreq)
  
}

