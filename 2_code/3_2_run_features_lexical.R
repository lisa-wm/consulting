# ------------------------------------------------------------------------------
# EXTRACTION OF LEXICAL FEATURES
# ------------------------------------------------------------------------------

# IN: data with topic labels
# OUT: data with lexical features



# first extract stuff like negation, then throw out and tokenize remainder
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

# Take any common German negation patterns

tokens_negation <- SnowballC::wordStem(c(
  "nicht", "nie", "niemals", "nein", "niemand", "nix", "nirgends", "kein"))

# Match documents with negation patterns

tweets_negation <- convert_dfm_to_dt(
  quanteda::dfm_match(tweets_dfm_basic, tokens_negation),
  key = "doc_id"
)

# Sum negations over all negation patterns

tweets_negation <- tweets_negation[
  , n_negations := sum(.SD),
  .SDcols = -c("doc_id"),
  by = doc_id
  ][, .(doc_id, n_negations)]

# EXTRACT PUNCTUATION ----------------------------------------------------------

# Take punctuation patterns deemed indicative for sentiments: 3 consecutive
# dots and one or multiple exclamation / question marks

tokens_punctuation <- c(
  dotdotdot = "[.]{3}", 
  exclamation_mark_single = "!", 
  question_mark_single = "\\?", 
  exclamation_mark_rep = "[!]{2}", 
  question_mark_rep = "[\\?]{2}")

# Match documents with punctuation patterns

tweets_punctuation <- convert_dfm_to_dt(
  quanteda::dfm_match(tweets_dfm_basic, tokens_punctuation),
  key = "doc_id")

data.table::setnames(tweets_punctuation, c("doc_id", names(tokens_punctuation)))

# EXTRACT REPEATED CHARACTER SEQUENCES -----------------------------------------

# Take single characters repeated at least 3 times and repated character
# sequences (such as "haha")

tokens_repetition <- c(
  repeated_char = "(.)\\1{2}", 
  repeated_char_seq = "\\b(\\S+?)\\1\\S*\\b")

tweets_repetition <- convert_dfm_to_dt(
  quanteda::dfm_match(tweets_dfm_basic, tokens_repetition),
  key = "doc_id")

data.table::setnames(tweets_repetition, c("doc_id", names(tokens_repetition)))

# TODO check whether there are really no matches

# EXTRACT N-GRAMS --------------------------------------------------------------

# Now that specific patterns, such as negations, have been extracted, stopwords
# may be removed

# Word unigrams

tweets_tokens_ngrams <- quanteda::tokens_select(
  quanteda::tokens_tolower(tweets_tokens_basic),
  pattern = make_stopwords_sa(),
  selection = "remove")

tweets_unigrams <- quanteda::tokens_select(
  tweets_tokens_ngrams,
  pattern = "[[:punct:]]",
  valuetype = "regex",
  selection = "remove"
)

tweets_unigrams <- convert_dfm_to_dt(
  quanteda::dfm_tfidf(
    quanteda::dfm_trim(
      quanteda::dfm(tweets_unigrams),
      min_docfreq = 0.005,
      docfreq_type = "prop")),
  key = "doc_id"
)

# Word bigrams

# Not worth it (even if min doc freq is set to 0.001, only 60 bigrams are found
# across docs)

# tweets_bigrams <- quanteda::tokens_ngrams(
#   tweets_unigrams,
#   n = 2L
# )

# Word skipgrams

# Not worth it (even if min doc freq is set to 0.001 and skips from 1 to 20 are
# allowed, only 50 skipgrams are found across docs)

# tweets_skipgrams <- quanteda::tokens_skipgrams(
#   tweets_unigrams,
#   n = 2L,
#   skip = 1L:20L
# )
# 
# tweets_dfm_skipgrams <- convert_dfm_to_dt(
#   quanteda::dfm_tfidf(
#     quanteda::dfm_trim(
#       quanteda::dfm(tweets_skipgrams),
#       min_docfreq = 0.001,
#       docfreq_type = "prop")),
#   key = "doc_id"
# )

# Character unigrams

tweets_char_unigrams <- quanteda::tokens(
  tweets_corpus,
  what = "character",
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_separators = TRUE,
  split_hyphens = TRUE) 

tweets_char_unigrams <- convert_dfm_to_dt(
  quanteda::dfm(tweets_char_unigrams),
  key = "doc_id"
)

# EXTRACT POS TAGS -------------------------------------------------------------

# TODO run this in set up

if (FALSE) spacy_install()
if (FALSE) spacy_download_langmodel("de")

if (FALSE) {
  
  spacy_initialize(model = "de_core_news_sm")
  
  tweets_corpus_tagged <- as.data.table(
    spacyr::spacy_parse(
      tweets_corpus,
      lemma = FALSE,
      entity = FALSE),
    key = "doc_id")
  
  save_rdata_files(tweets_corpus_tagged, folder = "2_code/3_sentiment_analysis")
  
}

load_rdata_files(tweets_corpus_tagged, folder = "2_code/3_sentiment_analysis")

tweets_dt_tagged <- tweets_corpus_tagged[
  , .(doc_id, pos)
  ][, n_tags := .N]

tweets_dt_tagged <- dcast(tweets_dt_tagged, doc_id ~ pos, value.var = "n_tags")

# COLLECT EXTRACTED FEATURES ---------------------------------------------------

tweets_features_lexical <- tweets_negation[
  tweets_punctuation, 
  ][tweets_repetition, 
    ][tweets_unigrams, 
      ][tweets_char_unigrams, 
        ][tweets_dt_tagged, ]

save_rdata_files(
  tweets_features_lexical, 
  folder = "2_code/3_sentiment_analysis")
