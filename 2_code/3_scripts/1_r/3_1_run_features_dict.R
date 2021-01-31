# ------------------------------------------------------------------------------
# EXTRACTION OF DICTIONARY FEATURES
# ------------------------------------------------------------------------------

# IN: topic-word distributions + publicly available sentiment dictionaries +
# data with topic labels
# OUT: dictionaries + data with dictionary-based features

# MAKE STANDARD SENTIMENT DICTIONARY -------------------------------------------

# Get dictionaries from various sources

dict_german_polarity_clues <- get_dict_gpc()
dict_sentiws <- get_dict_sentiws()
dict_rauh <- get_dict_rauh()

# TODO check whether strong weak is so beneficial, rauh does not fit in

# Create global dictionary

dict_global <- quanteda::dictionary(
  list(
    positive_global_strong = unique(c(
      dict_german_polarity_clues$positive[polarity_degree == "strong", term], 
      dict_sentiws$positive[polarity_degree == "strong", term])),
    negative_global_strong = unique(c(
      dict_german_polarity_clues$negative[polarity_degree == "strong", term], 
      dict_sentiws$negative[polarity_degree == "strong", term])),
    positive_global_weak = unique(c(
      dict_german_polarity_clues$positive[polarity_degree == "weak", term],
      dict_sentiws$positive[polarity_degree == "weak", term],
      dict_rauh[polarity == "positive", term])),
    negative_global_weak = unique(c(
      dict_german_polarity_clues$negative[polarity_degree == "weak", term],
      dict_sentiws$negative[polarity_degree == "weak", term],
      dict_rauh[polarity == "negative", term]))))

# LABEL WITH STANDARD POLARITIES -----------------------------------------------

load_rdata_files(tweets_corpus, folder = "2_code/1_data/2_tmp_data")
load_rdata_files(tweets_tokens_basic, folder = "2_code/1_data/2_tmp_data")

# Exclude some potentially sentiment-bearing words from stopwords list

stopwords_sa <- stringr::str_remove_all(
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

stopwords_sa <- stopwords_sa[nchar(stopwords_sa) > 0]

# Create dfm object

tweets_tokens_sa <- quanteda::tokens_remove(
  quanteda::tokens_tolower(tweets_tokens_basic),
  pattern = stopwords_sa) 

save_rdata_files(tweets_tokens_sa, folder = "2_code/1_data/2_tmp_data")

tweets_dfm_sa <- quanteda::dfm(tweets_tokens_sa)

save_rdata_files(tweets_dfm_sa, folder = "2_code/1_data/2_tmp_data")

# Match with polarities

tweets_sentiments_global <- convert_qtda_to_dt(
  quanteda::dfm_lookup(tweets_dfm_sa, dict_global),
  key = "doc_id")

# MAKE EMOJI DICTIONARY --------------------------------------------------------

# Read data

emojis_ranking <- data.table::fread(
  here("2_code/1_data/0_external_data", "emojis-sentiment-ranking.csv"),
  drop = c(1, 3, 4, 8, 9), 
  col.names = c(
    "unicode", 
    "negative", 
    "neutral",
    "positive"),
  encoding = "UTF-8")

# TODO Check whether this the right encoding

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
    positive_emojis = emojis_ranking[polarity == "positive", unicode],
    negative_emojis = emojis_ranking[polarity == "negative", unicode]))

# LABEL WITH EMOJI POLARITIES --------------------------------------------------

# Find tweets using emojis and convert those to int representation

load_rdata_files(data_clean, folder = "2_code/1_data/2_tmp_data")

tweets_emojis <- data_clean[
  , .(doc_id, emojis)
  ][lengths(emojis) > 0
    ][, emojis := paste(unlist(emojis), collapse = " "), by = doc_id
      ][, emojis := as.character(emojis)
        ][, emojis := paste(as.character(
          sapply(unlist(stringr::str_split(emojis, " ")), utf8ToInt)),
          collapse = " "),
          by = doc_id
          ][, n_emojis := stringr::str_count(emojis, "\\w+"), by = doc_id]

# Convert to dfm object

tweets_corpus_emojis <- quanteda::corpus(
  tweets_emojis, 
  docid_field = "doc_id",
  text_field = "emojis")

tweets_dfm_emojis <- quanteda::dfm(tweets_corpus_emojis)

# TODO look into that - non-matches veritable non-matches?

tweets_sentiments_emojis <- convert_qtda_to_dt(
  quanteda::dfm_lookup(tweets_dfm_emojis, dict_emojis),
  key = "doc_id")

tweets_sentiments_emojis <- 
  tweets_sentiments_emojis[tweets_emojis[, .(doc_id, n_emojis)]]

# MAKE TOPIC-SPECIFIC DICTIONARIES ---------------------------------------------

# TODO Set up

# MAKE TOPIC-SPECIFIC DICTIONARIES ---------------------------------------------

# TODO Set up

# COMBINE ALL DICTIONARY-BASED FEATURES ----------------------------------------

tweets_features_dict <- tweets_sentiments_emojis[
  tweets_sentiments_global, on = "doc_id"
  ][, c("positive_emojis", "negative_emojis", "n_emojis") :=
      lapply(.SD, function(i) {ifelse(is.na(i), 0, i)}),
    .SDcols = c("positive_emojis", "negative_emojis", "n_emojis"),
    by = "doc_id"]

save_rdata_files(tweets_features_dict, folder = "2_code/1_data/2_tmp_data")
