# ------------------------------------------------------------------------------
# CREATION OF A DOCUMENT-FEATURE MATRIX
# ------------------------------------------------------------------------------

# Purpose: create a document-feature matrix for sentiment analysis

# HELPER FUNCTIONS -------------------------------------------------------------



# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

make_dfm <- function(tokens, min_termfreq = 10) {
  
  quanteda::dfm(tokens, verbose = FALSE) %>% 
    quanteda::dfm_wordstem(language = "german") %>%
    quanteda::dfm_trim(min_termfreq = min_termfreq)
  
}

