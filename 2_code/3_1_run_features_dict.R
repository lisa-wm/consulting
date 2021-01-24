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

foo <- quanteda::dfm_lookup(tweets_dfm_sa, dict_sentiments)

sentiments_df <- convert(
  dfm_lookup(dfm, dictionary),
  to = "data.frame") %>% 
  as.data.table()

make_sentiments_dict <- function(dfm, dictionary) {
  
  sentiments_df <- convert(
    dfm_lookup(dfm, dictionary),
    to = "data.frame") %>% 
    as.data.table()
  
  sentiments_df[, `:=` (
    doc_id = as.numeric(doc_id),
    sentiments_found = positive + negative,
    diff_pos = positive - negative,
    label = case_when(
      positive - negative > 0 ~ "positive",
      positive - negative < 0 ~ "negative",
      positive - negative == 0 ~ "indecisive"))]
  
  median_sentiments_found <- median(sentiments_df[, sentiments_found])
  
  sentiments_df[, confidence_factor := 
                  sentiments_found / ..median_sentiments_found]
  
}

sentiments_dict_unigrams <- make_sentiments_dict(
  tweets_dfm_unigrams,
  global_dict_unigrams
)

# Append labels to data and save
# This may look the wrong way round but is actually data.table's way to perform
# a left join

data_labeled_dict_unigrams <- sentiments_dict_unigrams[
  data_processed, on = "doc_id"]

save(
  data_labeled_dict_unigrams,
  file = here("2_code", "rdata-tweets-labeled-dict-unigrams.RData"))
