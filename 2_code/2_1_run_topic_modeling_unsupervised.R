# ------------------------------------------------------------------------------
# TOPIC MODELING - UNSUPERVISED
# ------------------------------------------------------------------------------

# IN: single data file of cleaned tweets and meta data
# OUT: single data file of cleaned tweets and meta data with topic label + 
# file of topic-word distributions

# CONVERT TO TOKENS OBJECT -----------------------------------------------------

load(
  here(
    "2_code", 
    paste0("rdata_", as.character(bquote(tweets_corpus)), ".RData")))

tweets_tokens_tm <- quanteda::tokens(
  tweets_corpus,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_separators = TRUE,
  split_hyphens = TRUE,
  include_docvars = TRUE) 

# LOWERCASE TOKENS, REMOVE STOPWORDS AND PERFORM STEMMING

tweets_tokens_tm <- tweets_tokens_tm %>% 
  quanteda::tokens_tolower() %>% 
  quanteda::tokens_select(
    pattern = make_stopwords_tm(),
    selection = "remove"
  ) %>% 
  quanteda::tokens_wordstem(language = "german")




