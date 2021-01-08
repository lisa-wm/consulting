# ------------------------------------------------------------------------------
# PRE-PROCESSING TWEETS
# ------------------------------------------------------------------------------

# IN: raw twitter data + meta data
# OUT: single data file of cleaned tweets and meta data

# READ AND MERGE DATA ----------------------------------------------------------

# Read tweets

tweets_raw <- data.table::fread(
  here("1_scraping/output", "tweepy_df_subset_no_retweets.csv"), 
  encoding = "UTF-8",
  sep = ",")

# Discard non-German tweets and add unique document ID
# TODO Make language detection better

tweets_raw <- tweets_raw[
  cld3::detect_language(full_text) == "de"
  ][, doc_id := .I]

# Read meta data

meta_mp_level <- data.table::fread(
  here("1_scraping/output", "abg_twitter_df.csv"),
  encoding = "UTF-8",
  sep = ",",
  drop = "twitter")

meta_socio_electoral <- data.table::fread(
  here("1_scraping/output", "socioeconomics_zweitstimmen_df.csv"),
  encoding = "UTF-8",
  sep = ",",
  drop = c("district", "wahlkreis")
)

# Merge tweets and meta data

data_raw <- merge_tweets_meta(
  tweets_data = tweets_raw,
  mp_data = meta_mp_level,
  se_data = meta_socio_electoral
)

# REMOVE UMLAUTS AND NON-INFORMATIVE SYMBOLS -----------------------------------

data_clean <- copy(data_raw)[
  , full_text := remove_umlauts(full_text)
  ][, full_text := remove_noisy_symbols(full_text)]

# EXTRACT TWITTER-SPECIFIC ELEMENTS --------------------------------------------

# String pattern of emojis, hashtags and tags
# TODO remove mentions and hashtags from jupyter

# emoji_lexicon <- data.table::fread(
#   here(
#     "2_code/3_sentiment_analysis/1_dict_based/dicts", 
#     "emojis-sentiment-ranking.csv"), 
#   encoding = "UTF-8",
#   sep = ",")

pattern_emoji <- stringr::str_c(c(
  "[^\001-\177]", # unicode emojis
  "(\\:(\\-)?\\))", # simple happy smiley w/ or w/o nose
  "(\\:(\\-)?\\()", # simple sad smiley w/ or w/o nose
  "(\\;(\\-)?\\))", # simple winking smiley w/ or w/o nose
  "\\:P"), # simple smiley sticking tongue out
  collapse = "|")

pattern_hashtag <- "#\\S+"
pattern_tag <- "@\\S+"

patterns_to_remove <- stringr::str_c(
  c(pattern_emoji, "#", pattern_tag), # remove only hashtag symbol, not content
  collapse = "|")

# Extract/remove patterns and remove leading/trailing/double spaces

data_clean[, `:=` (
  emojis = stringr::str_extract_all(full_text, pattern_emoji), 
  hashtags = stringr::str_extract_all(full_text, pattern_hashtag),
  tags = stringr::str_extract_all(full_text, pattern_tag),
  full_text = stringr::str_remove_all(full_text, patterns_to_remove))
  ][, full_text := stringr::str_squish(full_text)]

# Split camel case sometimes used in hashtags

data_clean[, full_text := lapply(
  .I, function(i) {
    str_c(
      unlist(str_split(full_text[i], "(?<=[[:lower:]])(?=[[:upper:]])")),
      collapse = " ")})]  

# CREATE CORPUS OBJECT ---------------------------------------------------------

data_clean[, full_text := as.character(full_text)]

tweets_corpus <- quanteda::corpus(
  x = data_clean,
  docid_field = "doc_id",
  text_field = "full_text")

save(
  tweets_corpus, 
  file = here(
    "2_code", 
    paste0("rdata_", as.character(bquote(tweets_corpus)), ".RData")))
