# ------------------------------------------------------------------------------
# TOPIC MODELING - PREPARATION FOR BOTH UNSUPERVISED & KEYWORD-BASED VERSIONS
# ------------------------------------------------------------------------------

# IN: corpus object
# OUT: dfm object with original documents and topic modeling features 

# CONVERT TO TOKENS OBJECT -----------------------------------------------------

load_rdata_files(tweets_corpus, folder = "2_code")

tweets_tokens_tm <- quanteda::tokens(
  tweets_corpus,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_separators = TRUE,
  split_hyphens = TRUE,
  include_docvars = TRUE) 

# LOWERCASE TOKENS, REMOVE STOPWORDS AND PERFORM STEMMING ----------------------

# Standard stopwords removal and stemming, selection of words starting with 
# uppercase letters (assumption: nouns are more indicative of topics)

# !!! removed pipe op bc it is pretty but slooow

# TODO check out spacyr tagger for nouns

tweets_tokens_tm <- quanteda::tokens_wordstem(
  tweets_tokens_tm, 
  language = "german")

tweets_tokens_tm <- quanteda::tokens_select(
  tweets_tokens_tm,
  pattern = c("[[:upper:]]([[:lower:]])+"),
  valuetype = "regex",
  selection = "keep",
  case_insensitive = FALSE)

tweets_tokens_tm <- quanteda::tokens_select(
  quanteda::tokens_tolower(tweets_tokens_tm),
  pattern = make_stopwords_tm(),
  selection = "remove")

# CREATE DFM OBJECT ------------------------------------------------------------

# Create dfm (featuring only words of certain frequency and exclusiveness)

tweets_dfm_tm <- quanteda::dfm(tweets_tokens_tm)

save_rdata_files(tweets_dfm_tm, folder = "2_code")