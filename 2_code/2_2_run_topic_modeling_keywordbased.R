# ------------------------------------------------------------------------------
# TOPIC MODELING - KEYWORD-BASED
# ------------------------------------------------------------------------------

# IN: single data file of cleaned tweets and meta data + list of keywords
# OUT: single data file of cleaned tweets and meta data with topic label + 
# file of topic-word distributions

# CREATE FEATURES FOR TOPIC MODELING -------------------------------------------

load(
  here("2_code", paste0("rdata_", as.character(bquote(tweets_corpus)), ".RData")))

# Read tweets


