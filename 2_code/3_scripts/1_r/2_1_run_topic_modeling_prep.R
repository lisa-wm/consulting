# ------------------------------------------------------------------------------
# TOPIC MODELING - PREPARATION FOR BOTH UNSUPERVISED & KEYWORD-BASED VERSIONS
# ------------------------------------------------------------------------------

# IN: corpus object
# OUT: dfm object with original documents and topic modeling features 

# CONVERT TO TOKENS OBJECT -----------------------------------------------------

load_rdata_files(tweets_corpus, folder = "2_code/1_data/2_tmp_data")

# Create basic tokens object that will be used throughout the analysis w/
# further refinement in place

tweets_tokens_basic <- quanteda::tokens(
  tweets_corpus,
  what = "word",
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_separators = TRUE,
  split_hyphens = TRUE,
  include_docvars = TRUE
)

tweets_tokens_basic <- quanteda::tokens_wordstem(
  tweets_tokens_basic, 
  language = "german")

save_rdata_files(tweets_tokens_basic, folder = "2_code/1_data/2_tmp_data")

# EXTRACT TOKENS STARTING WITH UPPERCASE ---------------------------------------

# Assumption: nouns are more indicative of topical properties

tweets_tokens_tm <- quanteda::tokens_keep(
  tweets_tokens_basic,
  pattern = c("[[:upper:]]([[:lower:]])+"),
  valuetype = "regex",
  case_insensitive = FALSE)

# LOWERCASE TOKENS, REMOVE STOPWORDS AND PERFORM STEMMING ----------------------

# Lowercase and remove standard stopwords plus some additional, context-relevant
# stopwords

tweets_tokens_tm <- quanteda::tokens_remove(
  quanteda::tokens_tolower(tweets_tokens_tm),
  pattern = c(
    make_stopwords(),
    c("polit",
      "bundesregier",
      "bundestag",
      "deutsch",
      "deutschland",
      "berlin",
      "prozent",
      "herzlich",
      "glueckwunsch",
      "frag",
      "woch",
      "partei")))

# CREATE DFM OBJECT ------------------------------------------------------------

# Create dfm (featuring only words of certain frequency and exclusiveness)

tweets_dfm_tm <- quanteda::dfm(tweets_tokens_tm)

save_rdata_files(tweets_dfm_tm, folder = "2_code/1_data/2_tmp_data")
