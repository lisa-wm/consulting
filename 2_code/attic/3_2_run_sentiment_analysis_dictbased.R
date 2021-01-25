# ------------------------------------------------------------------------------
# SENTIMENT ANALYSIS - DICTIONARY-BASED
# ------------------------------------------------------------------------------

# IN: dfm object with original documents and topic modeling features +
# various dictionaries (standard, emoji and topic-specific)
# OUT: corpus object annotated with topic labels + file with topic variables

# CREATE DFM FOR SENTIMENT ANALYSIS --------------------------------------------

load_rdata_files(tweets_corpus_topics_keywordbased, folder = "2_code")
load_rdata_files(tweets_corpus_topics_unsupervised, folder = "2_code")

# Choose topic labels to work with

# corpus_sa <- tweets_corpus_topics_keywordbased
tweets_corpus_sa <- tweets_corpus_topics_unsupervised

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

tweets_dfm_sa <- quanteda::dfm(tweets_tokens_sa)

save_rdata_files(tweets_dfm_sa, folder = "2_code")

# CREATE DFM FOR SENTIMENT ANALYSIS --------------------------------------------


load_rdata_files(dict_sentiments, folder = "2_code/3_sentiment_analysis")
load_rdata_files(dict_emojis, folder = "2_code/3_sentiment_analysis")



# Find documents containing polarity terms

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
