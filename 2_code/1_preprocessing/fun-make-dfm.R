# ------------------------------------------------------------------------------
# CREATION OF A DOCUMENT-FEATURE MATRIX
# ------------------------------------------------------------------------------

# Purpose: create a document-feature matrix for sentiment analysis

# HELPER FUNCTIONS -------------------------------------------------------------

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

make_dfm <- function(tokens_ngrams, min_termfreq = 1) {
  
  quanteda::dfm(
    tokens_ngrams, 
    stem = TRUE,
    verbose = FALSE) %>% 
    # quanteda::dfm_wordstem(language = "german") %>% 
    quanteda::dfm_trim(min_termfreq = min_termfreq) %>% 
    quanteda::dfm_tfidf()

}
