# ------------------------------------------------------------------------------
# DICTIONARY EXPANSION BY TOPIC-SPECIFIC TERMS
# ------------------------------------------------------------------------------

# IN: topic-word distributions + publicly available sentiment dictionaries +
# data with topic labels
# OUT: dictionaries + dictionary-based data features

# MAKE STANDARD SENTIMENT DICTIONARY -------------------------------------------

# German polarity clues

german_polarity_clues <- get_dict_gpc()

#  SentiWS

senti_ws <- get_dict_sentiws()

# Dictionary

dict_sentiments <- quanteda::dictionary(
  list(
    positive = unique(c(german_polarity_clues$positive, senti_ws$positive)),
    negative = unique(c(german_polarity_clues$negative, senti_ws$negative))),
  tolower = TRUE)

save_rdata_files(dict_sentiments, folder = "2_code/3_sentiment_analysis")

# MAKE EMOJI DICTIONARY --------------------------------------------------------

emojis_ranking <- data.table::fread(
  here("2_code/0_external_data", "emojis-sentiment-ranking.csv"),
  drop = c(1, 9), 
  col.names = c(
    "unicode", 
    "occurrences", 
    "position", 
    "negative", 
    "neutral",
    "positive", 
    "name"))

# TODO Check whether this the right encoding

emojis_ranking[
  , principal_emotion := which.max(.SD),
  .SDcols = c("negative", "neutral", "positive"),
  by = seq_len(nrow(emojis_ranking))
  ][, polarity := data.table::fcase(
    principal_emotion == 1, "positive",
    principal_emotion == 2, "neutral",
    principal_emotion == 3, "negative")]

dict_emojis <- quanteda::dictionary(
  list(
    positive = emojis_ranking[polarity == "positive", .(unicode)],
    negative = emojis_ranking[polarity == "negative", .(unicode)]),
  tolower = FALSE)

save_rdata_files(dict_emojis, folder = "2_code/3_sentiment_analysis")

# MAKE TOPIC-SPECIFIC DICTIONARIES ---------------------------------------------

# TODO Set up

# CONVERT DATA TO DFM OBJECT ---------------------------------------------------

load_rdata_files(tweets_corpus_topics_unsupervised, folder = "2_code")

tweets_corpus_sa <- tweets_corpus_topics_unsupervised

# Tokenize for sentiment analysis

tweets_tokens_sa <- quanteda::tokens(
  tweets_corpus_sa,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_separators = TRUE,
  split_hyphens = TRUE,
  include_docvars = TRUE) 

tweets_tokens_sa <- tweets_tokens_sa %>% 
  quanteda::tokens_wordstem(language = "german") %>% 
  quanteda::tokens_tolower() %>%
  quanteda::tokens_select(
    pattern = make_stopwords_sa(),
    selection = "remove") 

# Convert to dfm object

tweets_dfm_sa <- quanteda::dfm(tweets_tokens_sa)

save_rdata_files(tweets_dfm_sa, folder = "2_code")

# MATCH POLARITY WORDS IN DOCUMENTS --------------------------------------------

load_rdata_files(data_clean, folder = "2_code")

# Extract texts

tweets_texts <- data_clean[, .(doc_id, full_text)]

# Find documents using emojis and convert to unicode

tweets_emojis <- data_clean[
  , .(doc_id, emojis)
  ][, n_emojis := lengths(emojis)
    ][n_emojis > 0
      ][, n_emojis := NULL
        ][, emojis := paste(unlist(emojis), collapse = " "), by = doc_id
          ][, emojis := as.character(emojis)
          ][, emojis := iconv(emojis, "", "ASCII", "Unicode")]

tweets_corpus_emojis <- quanteda::corpus(
  tweets_emojis, 
  docid_field = "doc_id",
  text_field = "emojis")

# tweets_dfm_emojis <- quanteda::dfm(tweets_corpus_emojis)
# 
# # Combine
# 
# tweets_lookup <- tweets_emojis[tweets_texts]
# 
# # Look up in dictionaries
# 
# tweets_global_sentiments <- setDT(
#   quanteda::convert(
#     quanteda::dfm_lookup(tweets_dfm_sa, dict_sentiments),
#     to = "data.frame"))
# 
# 
# 
# tweets_emojis <- data_clean[, .(doc_id, emojis)][
#   , list(emojis = as.character(unlist(emojis))), 
#   by = doc_id]
# 
# 
# 
# tweets_emojis <- data_clean[1:50][
#   , list(emojis = as.character(unlist(emojis))), 
#   by = doc_id
#   ]
# 
# sapply(tweets_emojis$emojis, stringi::stri_trans_general, "Hex-Any/Unicode")
# 
# foo <- quanteda::corpus()
# 
# foo <- data_clean[
#   , .(doc_id, emojis)
#   ][, emojis := unlist(emojis)]
# 
# foo <- as.matrix(data_clean[, .(doc_id, emojis)])
# foo_dfm <- quanteda::as.dfm(foo)
# 
# tweets_emoji_sentiments <- as.data.table(
#   quanteda::convert(
#     quanteda::dfm_lookup(tweets_dfm_sa, dict_emojis)
#   )
# )