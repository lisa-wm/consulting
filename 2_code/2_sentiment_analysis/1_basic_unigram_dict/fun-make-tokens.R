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

get_noise_tokens <- function() {
  
  c("@*",
    "*innen"
    )
  
}

sentence <- "John Doe is a dumbass whose significance I never got. Full stop."

tokens(sentence, remove_punct = TRUE)

# TODO Set up stemming and lemmatization

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

make_tokens <- function(corpus) {
  
  toks <- tokens(
    corpus,
    remove_punct = TRUE, 
    remove_numbers = TRUE,
    verbose = FALSE) 
  
  tokens_tolower(toks) %>%
    tokens_select(min_nchar = 4, ) %>% 
    tokens_remove(c(
      get_stopwords(), 
      "@*", 
      "*innen",
      "^(polit|bundesregier|bundestag|deutsch|land|jaehrig)"
      ))
    
}

make_tokens(tweets_corpus[1:10])
