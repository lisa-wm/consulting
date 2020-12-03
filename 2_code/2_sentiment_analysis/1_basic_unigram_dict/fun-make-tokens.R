# ------------------------------------------------------------------------------
# TOKENIZATION OF CORPUS
# ------------------------------------------------------------------------------

# Purpose: create tokens out of basic corpus

# HELPER FUNCTIONS -------------------------------------------------------------

# TODO Set up stemming and lemmatization

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

make_tokens <- function(corpus, stopwords) {
  
  toks <- quanteda::tokens(
    corpus,
    remove_punct = TRUE, 
    remove_numbers = TRUE,
    verbose = FALSE) 
  
  quanteda::tokens_tolower(toks) %>%
    quanteda::tokens_select(min_nchar = 4) %>% 
    quanteda::tokens_remove(c(stopwords, "*innen")) %>% # general noise
    quanteda::tokens_remove( # context-specific noise
      stringr::str_c(c(
        "^(polit",
        "bundesregier",
        "bundestag",
        "deutsch",
        "berlin",
        "prozent)"), 
        collapse = "|"), 
      valuetype = "regex")
    
}

