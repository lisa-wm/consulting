# ------------------------------------------------------------------------------
# EXTRACTION OF LEXICAL FEATURES
# ------------------------------------------------------------------------------

# IN: data with topic labels
# OUT: data with lexical features

# EXTRACT NEGATIONS ------------------------------------------------------------

load_rdata_files(tweets_dfm_sa, folder = "2_code/1_data/2_tmp_data")

# Take any common German negation patterns

tokens_negation <- SnowballC::wordStem(
  remove_umlauts(c(
    "nicht", 
    "nie", 
    "niemals", 
    "nein", 
    "niemand", 
    "nix", 
    "nirgends", 
    "kein")),
  language = "de")

# Match documents with negation patterns

tweets_negation <- convert_qtda_to_dt(
  quanteda::dfm_match(tweets_dfm_sa, tokens_negation),
  key = "doc_id")

# Sum negations over all negation patterns

tweets_negation <- tweets_negation[
  , n_negations := sum(.SD),
  .SDcols = -c("doc_id"),
  by = doc_id
  ][, .(doc_id, n_negations)]

# EXTRACT INTENSIFIERS ---------------------------------------------------------

# Take any common German intensification patterns

tokens_intensification <- SnowballC::wordStem(
  remove_umlauts(c(
    "sehr", 
    "besonders", 
    "total", 
    "absolut", 
    "völlig", 
    "enorm", 
    "maximal")),
  language = "de")

# Match documents with intensification patterns

tweets_intensification <- convert_qtda_to_dt(
  quanteda::dfm_match(tweets_dfm_sa, tokens_intensification),
  key = "doc_id")

# Sum intensifications over all intensification patterns

tweets_intensification <- tweets_intensification[
  , n_intensifications := sum(.SD),
  .SDcols = -c("doc_id"),
  by = doc_id
  ][, .(doc_id, n_intensifications)]

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

tweets_punctuation <- convert_qtda_to_dt(
  quanteda::dfm_match(tweets_dfm_sa, tokens_punctuation),
  key = "doc_id")

data.table::setnames(tweets_punctuation, c("doc_id", names(tokens_punctuation)))

# EXTRACT REPEATED CHARACTER SEQUENCES -----------------------------------------

# Take single characters repeated at least 3 times and repated character
# sequences (such as "haha")

tokens_repetition <- c(
  repeated_char = "(.)\\1{2}", 
  repeated_char_seq = "\\b(\\S+?)\\1\\S*\\b")

tweets_repetition <- convert_qtda_to_dt(
  quanteda::dfm_match(tweets_dfm_sa, tokens_repetition),
  key = "doc_id")

data.table::setnames(tweets_repetition, c("doc_id", names(tokens_repetition)))

# TODO check whether there are really no matches

# EXTRACT N-GRAMS --------------------------------------------------------------

# Now that specific patterns, such as negations, have been extracted, stopwords
# may be removed

# Word unigrams

load_rdata_files(tweets_tokens_sa, folder = "2_code/1_data/2_tmp_data")

tweets_unigrams <- quanteda::tokens_remove(
  tweets_tokens_sa,
  pattern = "[[:punct:]]",
  valuetype = "regex")

tweets_unigrams <- convert_qtda_to_dt(
  quanteda::dfm_tfidf(
    quanteda::dfm_trim(
      quanteda::dfm(tweets_unigrams),
      min_docfreq = 0.005,
      docfreq_type = "prop")),
  key = "doc_id")

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

load_rdata_files(tweets_corpus, folder = "2_code/1_data/2_tmp_data")

tweets_char_unigrams <- quanteda::tokens(
  tweets_corpus,
  what = "character",
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_separators = TRUE,
  split_hyphens = TRUE) 

tweets_char_unigrams <- convert_qtda_to_dt(
  quanteda::dfm(tweets_char_unigrams),
  key = "doc_id")

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
  
  save_rdata_files(
    tweets_corpus_tagged, 
    folder = "2_code/1_data/2_tmp_data")
  
}

load_rdata_files(tweets_corpus_tagged, folder = "2_code/1_data/2_tmp_data")

tweets_pos_tags <- tweets_corpus_tagged[
  , .(doc_id, pos)
  ][, aux := 1L
    ][, n_tags := sum(aux), by = list(doc_id, pos)
      ][, aux := NULL]

tweets_pos_tags <- data.table::dcast(
  unique(tweets_pos_tags), 
  doc_id ~ pos, 
  value.var = "n_tags",
  fun.aggregate = sum)

data.table::setnames(tweets_pos_tags, tolower(names(tweets_pos_tags)))

tweets_pos_tags <- tweets_pos_tags[
  , .(doc_id, adj, adv, cconj, noun, propn, verb)]

# COLLECT FEATURES -------------------------------------------------------------

tweets_features_lexical <- tweets_pos_tags[
  tweets_negation
  ][tweets_intensification,
    ][tweets_punctuation,
      ][tweets_repetition,
        ][tweets_unigrams,
          ][tweets_char_unigrams, ]

save_rdata_files(tweets_features_lexical, folder = "2_code/1_data/2_tmp_data")
