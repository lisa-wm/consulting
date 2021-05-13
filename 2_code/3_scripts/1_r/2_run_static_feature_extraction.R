# ------------------------------------------------------------------------------
# STATIC FEATURE EXTRACTION
# ------------------------------------------------------------------------------

# IN: corpus object of cleaned tweets and meta data
# OUT: corpus object of cleaned tweets, meta data and static features

# EXTRACT TWITTER-SPECIFIC FEATURES --------------------------------------------

load_rdata_files(tweets_corpus, folder = "2_code/1_data/2_tmp_data")

tweets_features_twitter <- convert_qtda_to_dt(
  tweets_corpus,
  key = "doc_id")

tweets_features_twitter <- tweets_features_twitter[
  , .(doc_id, 
      twitter_hashtags, 
      twitter_tags, 
      feat_retweet_count = twitter_retweet_count, 
      feat_favorite_count = twitter_favorite_count)
  ][, `:=` (
    feat_n_hashtags = lengths(twitter_hashtags),
    feat_n_tags = lengths(twitter_tags)),
    by = doc_id
    ][, `:=` (twitter_hashtags = NULL, twitter_tags = NULL)]

save_rdata_files(tweets_features_twitter, folder = "2_code/1_data/2_tmp_data")

# EXTRACT DICTIONARY-BASED FEATURES --------------------------------------------

# Get polarity dictionaries from various sources

dict_gpc <- get_dict_gpc()
dict_sentiws <- get_dict_sentiws()
dict_rauh <- get_dict_rauh()

# Create global dictionary

dict_global <- quanteda::dictionary(
  list(
    feat_polarity_positive_strong = unique(c(
      dict_gpc$positive[polarity_degree == "strong", term], 
      dict_sentiws$positive[polarity_degree == "strong", term])),
    feat_polarity_negative_strong = unique(c(
      dict_gpc$negative[polarity_degree == "strong", term], 
      dict_sentiws$negative[polarity_degree == "strong", term])),
    feat_polarity_positive_weak = unique(c(
      dict_gpc$positive[polarity_degree == "weak", term],
      dict_sentiws$positive[polarity_degree == "weak", term],
      dict_rauh[polarity == "positive", term])),
    feat_polarity_negative_weak = unique(c(
      dict_gpc$negative[polarity_degree == "weak", term],
      dict_sentiws$negative[polarity_degree == "weak", term],
      dict_rauh[polarity == "negative", term]))))

# Create tokens object (keeping case and punctuation for the moment, will be
# needed later on)

tweets_tokens_basic <- quanteda::tokens(
  tweets_corpus,
  what = "word",
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_separators = TRUE,
  split_hyphens = TRUE,
  include_docvars = TRUE)

tweets_tokens_basic <- quanteda::tokens_wordstem(
  tweets_tokens_basic, 
  language = "german")

save_rdata_files(tweets_tokens_basic, folder = "2_code/1_data/2_tmp_data")

# Customize stopwords list, excluding some potentially sentiment-bearing words

stopwords_sfe <- stringr::str_remove_all(
  make_stopwords(),
  stringr::str_c(unique(SnowballC::wordStem(remove_umlauts(tolower(c(
    "gegen",
    "kein",
    "nicht",
    "sehr",
    "besser",
    "beste",
    "gut",
    "gern",
    "kaum",
    "nein",
    "nie",
    "niemand",
    "richtig",
    "schlecht"))),
    language = "de")),
    collapse = "|"))

stopwords_sfe <- stopwords_sfe[nchar(stopwords_sfe) > 0]

# Create dfm object

tweets_tokens_sfe <- quanteda::tokens_remove(
  quanteda::tokens_tolower(tweets_tokens_basic),
  pattern = stopwords_sfe) 

tweets_dfm_sfe <- quanteda::dfm(tweets_tokens_sfe)

save_rdata_files(tweets_tokens_sfe, folder = "2_code/1_data/2_tmp_data")
save_rdata_files(tweets_dfm_sfe, folder = "2_code/1_data/2_tmp_data")

# Match with polarities

tweets_sentiments_global <- convert_qtda_to_dt(
  quanteda::dfm_lookup(tweets_dfm_sfe, dict_global),
  key = "doc_id")

# ------------------------------------------------------------------------------

# Read emoji ranking data

emojis_ranking <- data.table::fread(
  here::here("2_code/1_data/0_external_data", "emojis-sentiment-ranking.csv"),
  drop = c(1L, 3L, 4L, 8L, 9L), 
  col.names = c(
    "unicode", 
    "negative", 
    "neutral",
    "positive"),
  encoding = "UTF-8")

# Transform to appropriate format and convert to int representation to match
# with tweets

emojis_ranking[
  , principal_emotion := which.max(.SD),
  .SDcols = c("negative", "neutral", "positive"),
  by = seq_len(nrow(emojis_ranking))
  ][, polarity := data.table::fcase(
    principal_emotion == 1, "negative",
    principal_emotion == 2, "neutral",
    principal_emotion == 3, "positive")
    ][, unicode := as.character(utf8ToInt(intToUtf8(unicode)))]

dict_emojis <- quanteda::dictionary(
  list(
    feat_positive_emojis = emojis_ranking[polarity == "positive", unicode],
    feat_negative_emojis = emojis_ranking[polarity == "negative", unicode]))

# Find tweets using emojis and convert those to integer representation

data_dt <- data.table::as.data.table(
  cbind(
    doc_id = quanteda::docid(tweets_corpus),
    quanteda::docvars(tweets_corpus)), 
  key = "doc_id")

tweets_emojis <- data_dt[
  , .(doc_id, twitter_emojis)
    ][, twitter_emojis := paste(unlist(twitter_emojis), collapse = " "), 
      by = doc_id
      ][, twitter_emojis := as.character(twitter_emojis)
        ][, twitter_emojis := paste(as.character(
          sapply(unlist(stringr::str_split(twitter_emojis, " ")), utf8ToInt)),
          collapse = " "),
          by = doc_id]

# Convert to dfm object

tweets_corpus_emojis <- quanteda::corpus(
  tweets_emojis, 
  docid_field = "doc_id",
  text_field = "twitter_emojis")

tweets_dfm_emojis <- quanteda::dfm(tweets_corpus_emojis)

# Match with dictionary

tweets_sentiments_emojis <- convert_qtda_to_dt(
  quanteda::dfm_lookup(tweets_dfm_emojis, dict_emojis),
  key = "doc_id")

tweets_sentiments_emojis[
  , feat_n_emojis := feat_positive_emojis + feat_negative_emojis, by = doc_id]

# ------------------------------------------------------------------------------

# Combine dictionary-based features

tweets_features_dict <- tweets_sentiments_emojis[
  tweets_sentiments_global, on = "doc_id"]

save_rdata_files(tweets_features_dict, folder = "2_code/1_data/2_tmp_data")

# EXTRACT LEXICAL FEATURES -----------------------------------------------------

# Define patterns for negations, intensifications, punctuations

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

tokens_intensification <- SnowballC::wordStem(
  remove_umlauts(c(
    "sehr", 
    "extrem",
    "besonders", 
    "total", 
    "absolut", 
    "völlig", 
    "enorm", 
    "maximal")),
  language = "de")

tokens_punctuation <- c(
  exclamation_mark = "!", 
  question_mark = "?")

# Match patterns

tweets_negation <- convert_qtda_to_dt(
  quanteda::dfm_match(tweets_dfm_sfe, tokens_negation),
  key = "doc_id")[
    , feat_n_negations := sum(.SD),
    .SDcols = -c("doc_id"),
    by = doc_id
    ][, .(doc_id, feat_n_negations)]

tweets_intensification <- convert_qtda_to_dt(
  quanteda::dfm_match(tweets_dfm_sfe, tokens_intensification),
  key = "doc_id")[
    , feat_n_intensifications := sum(.SD),
    .SDcols = -c("doc_id"),
    by = doc_id
    ][, .(doc_id, feat_n_intensifications)]

tweets_punctuation <- convert_qtda_to_dt(
  quanteda::dfm_match(tweets_dfm_sfe, tokens_punctuation),
  key = "doc_id")

data.table::setnames(
  tweets_punctuation, 
  c("doc_id", sprintf("feat_%s", names(tokens_punctuation))))

# Get character unigrams

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

data.table::setnames(
  tweets_char_unigrams, 
  c("doc_id",
    sprintf(
      "feat_%s", 
      names(tweets_char_unigrams)[names(tweets_char_unigrams) != "doc_id"])))

# EXTRACT POS TAGS -------------------------------------------------------------

# Commented because it takes some time

if (FALSE) spacyr::spacy_install()
if (FALSE) spacyr::spacy_download_langmodel("de")

if (FALSE) {
  
  spacyr::spacy_initialize(model = "de_core_news_sm")
  
  tweets_corpus_tagged <- data.table::as.data.table(
    spacyr::spacy_parse(
      tweets_corpus,
      lemma = FALSE,
      entity = FALSE),
    key = "doc_id")
  
  save_rdata_files(tweets_corpus_tagged, folder = "2_code/1_data/2_tmp_data")
  
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

tweets_pos_tags <- tweets_pos_tags[
  , .(doc_id, ADJ, ADV, CCONJ, NOUN, PROPN, VERB)]

data.table::setnames(
  tweets_pos_tags, 
  c("doc_id", 
    sprintf(
      "feat_%s",
      tolower(
        names(tweets_pos_tags)[names(tweets_pos_tags) != "doc_id"]))))

# COLLECT ALL STATIC FEATURES --------------------------------------------------

tweets_features_static <- tweets_pos_tags[
  tweets_negation
  ][tweets_intensification,
    ][tweets_punctuation,
      ][tweets_char_unigrams,
        ][tweets_features_twitter,
          ][tweets_features_dict, ]

save_rdata_files(tweets_features_static, folder = "2_code/1_data/2_tmp_data")

quanteda::docvars(tweets_corpus) <- as.data.frame(
  tweets_features_static[data_dt, on = "doc_id"])

save_rdata_files(tweets_corpus, folder = "2_code/1_data/2_tmp_data")
