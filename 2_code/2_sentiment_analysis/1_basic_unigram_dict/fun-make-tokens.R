# ------------------------------------------------------------------------------
# TOKENIZATION OF CORPUS
# ------------------------------------------------------------------------------

# Purpose: create tokens out of basic corpus

# HELPER FUNCTIONS -------------------------------------------------------------

# TODO Make good stopword list

get_stopwords <- function() {
  
  # Collect stopwords from various sources
  
  sw_1 <- stopwords("de")
  
  sw_2 <- xmlToDataFrame(xmlParse(here(
      "2_code/2_sentiment_analysis/1_basic_unigram_dict/dicts", 
      "german_stopwords.xml"), 
    encoding = "UTF-8")) %>% 
    unlist()

  sw_3 <- read.delim(here(
    "2_code/2_sentiment_analysis/1_basic_unigram_dict/dicts", 
    "stopwords-iso.txt"), encoding = "UTF-8") %>% 
    unlist()

  # Collect all and remove duplicates as well as umlauts

  find_this <- apropos("^sw_[1-99]")
  look_here <- sys.frame(sys.parent(0))
  
  stopwords <- unique(
    remove_umlauts(
      unlist(mget(find_this, envir = look_here)))
  )
    
  # Remove words deemed important for sentiment
  
  stopwords <- stringr::str_remove_all(
    stopwords, 
    pattern = stringr::str_c(c(
      "gegen", 
      "^kein",
      "^nicht",
      "sehr",
      "^besser",
      "^beste",
      "^gut",
      "^gern",
      "kaum",
      "nein",
      "nie",
      "^niemand",
      "^richtig",
      "^schlecht"),
    collapse = "|"))
  
  sort(stopwords)

}

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
    quanteda::tokens_remove(c( # general noise
      stopwords, 
      "@*", 
      "*innen")) %>% 
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

