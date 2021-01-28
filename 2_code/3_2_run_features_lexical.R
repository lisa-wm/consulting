# ------------------------------------------------------------------------------
# EXTRACTION OF LEXICAL FEATURES
# ------------------------------------------------------------------------------

# IN: data with topic labels
# OUT: data with lexical features



# first extract stuff like negation, then throw out and tokenize remainder
# negations
# exclamation marks etc.
# repeated char seqs such as hahaha?
# n-grams
# char k-grams
# skip grams?
# pos tags



# CREATE BASIC DFM -------------------------------------------------------------

# TODO check whether this could also be solved w/ bigrams / skipgrams

load_rdata_files(tweets_corpus, folder = "2_code")

tweets_tokens_basic <- quanteda::tokens(
  tweets_corpus,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_separators = TRUE,
  split_hyphens = TRUE) 

tweets_tokens_basic <- quanteda::tokens_wordstem(
  tweets_tokens_basic, 
  language = "german")

tweets_dfm_basic <- quanteda::dfm(tweets_tokens_basic)

# EXTRACT NEGATIONS ------------------------------------------------------------

tokens_negation <- SnowballC::wordStem(c(
  "nicht", "nie", "niemals", "nein", "niemand", "nix", "nirgends", "kein"))

tweets_negations <- as.data.table(
  quanteda::convert(
    quanteda::dfm_match(tweets_dfm_basic, tokens_negation), 
    to = "data.frame"),
  key = "doc_id")

tweets_negations <- tweets_negations[
  , n_negations := sum(.SD),
  .SDcols = -c("doc_id"),
  by = doc_id
  ][, .(doc_id, n_negations)]

# EXTRACT PUNCTUATION ----------------------------------------------------------

tokens_punctuation <- c(
  dotdotdot = "[.]{3}", 
  exclamation_mark_single = "!", 
  question_mark_single = "\\?", 
  exclamation_mark_rep = "[!]{2}", 
  question_mark_rep = "[\\?]{2}")

tweets_punctuation <- as.data.table(
  quanteda::convert(
    quanteda::dfm_match(tweets_dfm_basic, tokens_punctuation), 
    to = "data.frame"),
  key = "doc_id")

data.table::setnames(tweets_punctuation, c("doc_id", names(tokens_punctuation)))

