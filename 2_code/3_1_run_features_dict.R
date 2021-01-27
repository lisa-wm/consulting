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
    "name"),
  encoding = "UTF-8")

# TODO Check whether this the right encoding

emojis_ranking[
  , principal_emotion := which.max(.SD),
  .SDcols = c("negative", "neutral", "positive"),
  by = seq_len(nrow(emojis_ranking))
  ][, polarity := data.table::fcase(
    principal_emotion == 1, "negative",
    principal_emotion == 2, "neutral",
    principal_emotion == 3, "positive")]

dict_emojis <- quanteda::dictionary(
  list(
    positive = emojis_ranking[polarity == "positive", unicode],
    negative = emojis_ranking[polarity == "negative", unicode]),
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

tweets_tokens_sa <- quanteda::tokens_wordstem(
  quanteda::tokens_tolower(tweets_tokens_sa),
  language = "german")

tweets_tokens_sa <- quanteda::tokens_select(
  tweets_tokens_sa,
  pattern = make_stopwords_sa(),
  selection = "remove") 

# Convert to dfm object

tweets_dfm_sa <- quanteda::dfm(tweets_tokens_sa)

save_rdata_files(tweets_dfm_sa, folder = "2_code")

# MATCH POLARITY WORDS IN DOCUMENTS --------------------------------------------

tweets_sentiments_global <- setDT(
  quanteda::convert(
    quanteda::dfm_lookup(tweets_dfm_sa, dict_sentiments),
    to = "data.frame"))

# MATCH EMOJIS -----------------------------------------------------------------

# Find documents using emojis and convert to unicode

lex_pos <- emojis_ranking[polarity == "positive", unicode]
print(intToUtf8(lex_pos))

zwinker_smiley <- lex_pos[16]
zwinker_smiley_orig <- foo$emojis[5]
stringi::stri_trans_general(zwinker_smiley, "Any-Hex/Unicode")
stringi::stri_trans_general(zwinker_smiley_orig, "Any-Hex")
stringi::stri_trans_general(foo$emojis, "Any-Hex")

utf8ToInt(intToUtf8(0x1f609))
utf8ToInt(zwinker_smiley_orig)

x <- c(0xD801, 0xDC37)
intToUtf8(x)

foo <- data_clean[
  , .(doc_id, emojis)
][, n_emojis := lengths(emojis)
][n_emojis > 0
][, n_emojis := NULL
][, emojis := paste(unlist(emojis), collapse = " "), by = doc_id
][, emojis := as.character(emojis)
]#[, emojis := utf8ToInt(emojis)]


foo <- data_clean[
  , .(doc_id, emojis)
][, n_emojis := lengths(emojis)
][n_emojis > 0
][, n_emojis := NULL
][, emojis := paste(unlist(emojis), collapse = " "), by = doc_id
][, emojis := as.character(emojis)
][, emojis := paste(as.character(
  sapply(unlist(str_split(emojis, " ")), utf8ToInt)), collapse = " "), by = doc_id]

poo <- sapply(sapply(lex_pos, intToUtf8), utf8ToInt)

intersect(foo$emojis, poo)

c("Latin-ASCII", "Any-Hex/Unicode", "Hex-Any/Unicode")

load_rdata_files(data_clean, folder = "2_code")

tweets_emojis <- data_clean[
  , .(doc_id, emojis)
  ][, n_emojis := lengths(emojis)
    ][n_emojis > 0
      ][, n_emojis := NULL
        ][, emojis := paste(unlist(emojis), collapse = " "), by = doc_id
          ][, emojis := as.character(emojis)
            ][, emojis := iconv(emojis, "", "ASCII", "Unicode")]

test <- tweets_emojis$emojis[1:200]
a <- stringi::stri_trans_general(test, "hex")
test_2 <- emojis_ranking[polarity == "positive", unicode]

iconv(test_2, "", "ASCII", "Unicode")

iconv(test, "Unicode", "byte")

tweets_corpus_emojis <- quanteda::corpus(
  tweets_emojis, 
  docid_field = "doc_id",
  text_field = "emojis")

tweets_dfm_emojis <- quanteda::dfm(tweets_corpus_emojis)

# FIXME not working, emojis are encoded in different ways

tweets_sentiments_emojis <- setDT(
  quanteda::convert(
    quanteda::dfm_lookup(tweets_dfm_emojis, dict_emojis),
    to = "data.frame"))

utf8_normalize(test_2)


iconv(emojis_ranking[polarity == "positive", unicode], "", "ASCII", "Unicode")
featnames(tweets_dfm_emojis)
