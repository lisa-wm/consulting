# ------------------------------------------------------------------------------
# PRE-PROCESSING TWEETS
# ------------------------------------------------------------------------------

# IN: raw twitter data + meta data
# OUT: single data file of cleaned tweets and meta data

# READ AND CLEAN DATA ----------------------------------------------------------

# Read tweets
# Attention, these are stored in time-stamped folder to keep downloads from 
# different time points apart --> adjust

tweets_raw_new <- data.table::fread(
  here("1_scraping/output/202010113_2146", "tweepy_df_subset_no_retweets.csv"), 
  encoding = "UTF-8",
  sep = ",")

tweets_raw_new <- tweets_raw_new[created_at >= "2017-09-24"]

# Discard non-German tweets

tweets_raw_new <- tweets_raw_new[cld3::detect_language(full_text) == "de"]

# Append annotated data

tweets_raw_new[
  , `:=` (label = "none", topic = "none")
  ][, created_at := as.POSIXct(as.Date(created_at))]

# load_rdata_files(
  # data_labeled,
  # folder = "2_code/1_data/1_training_data",
  # tmp = FALSE)

tweets_raw_labeled <- data.table::fread(
  here("2_code/1_data/1_training_data", "data_labeled.csv"),
  encoding = "UTF-8",
  sep = ";")

tweets_raw_labeled[
  , created_at := as.POSIXct(created_at, format = "%d.%m.%Y %H:%M")]

data.table::setcolorder(tweets_raw_labeled, names(tweets_raw_new))  

tweets_raw_unlabeled <- which(
  !tweets_raw_new$full_text %in% tweets_raw_labeled$full_text)

tweets_raw <- unique(rbind(
  tweets_raw_new[tweets_raw_unlabeled], 
  tweets_raw_labeled))

tweets_raw <- tweets_raw[created_at > "2017-09-24"]

# Save for seminar

data.table::fwrite(
  tweets_raw[label != "none"],
  here("5_seminar", "twitter_data.csv"),
  sep = ";")

# Add word count and date variables

tweets_raw[, `:=` (
  word_count = quanteda::ntoken(full_text, remove_punct = TRUE),
  year = data.table::year(created_at),
  month = data.table::month(created_at),
  week = data.table::week(created_at))]

# Remove umlauts and non-informative symbols

tweets_raw[
  , full_text := remove_noisy_symbols(remove_umlauts(full_text))
  ][, name_matching := remove_noisy_symbols(remove_umlauts(name_matching))]

# Prefix column names and save

data.table::setcolorder(tweets_raw, c("label", "topic"))

data.table::setnames(
  tweets_raw, 
  c("label",
    "topic",
    sprintf(
      "twitter_%s", 
      names(tweets_raw)[!names(tweets_raw) %in% c("label", "topic")])))

save_rdata_files(
  tweets_raw,
  folder = "2_code/1_data/2_tmp_data")

# READ META DATA ---------------------------------------------------------------

meta_mp_level <- data.table::fread(
  here("1_scraping/output", "abg_twitter_df.csv"),
  encoding = "UTF-8",
  sep = ",",
  drop = "twitter",
  key = "name_matching")

meta_mp_level[, `:=` (
  name_matching = remove_noisy_symbols(remove_umlauts(name_matching)),
  bundesland = remove_noisy_symbols(remove_umlauts(bundesland)),
  wahlkreis = remove_noisy_symbols(remove_umlauts(wahlkreis)))]

data.table::setnames(
  meta_mp_level, 
  sprintf("meta_%s", names(meta_mp_level)))

meta_socio_electoral <- data.table::fread(
  here("1_scraping/output", "socioeconomics_zweitstimmen_df.csv"),
  encoding = "UTF-8",
  sep = ",",
  drop = c("district", "wahlkreis", "bundesland"),
  key = "wahlkreis_nr")

data.table::setnames(
  meta_socio_electoral, 
  sprintf("meta_%s", names(meta_socio_electoral)))

# MERGE TWEETS AND META DATA ---------------------------------------------------

# Mind order of join: to perform a left join of dt w/ dt2, use dt2[dt]

data_clean <- meta_mp_level[
  meta_socio_electoral, on = "meta_wahlkreis_nr"
  ][tweets_raw, on = c(meta_name_matching = "twitter_name_matching")
    ][, meta_party := ifelse(
      stringr::str_detect(meta_party, "\\*"),
      stringr::str_trim(stringr::str_replace_all(meta_party, "\\*", "")),
      meta_party)
      ][, `:=` (
        meta_bundesland = as.factor(meta_bundesland),
        meta_party = as.factor(meta_party))
        ][, `:=` (
          twitter_time_index_month = frank(
            list(twitter_year, twitter_month), 
            ties.method = "dense"),
          twitter_time_index_week = frank(
            list(twitter_year, twitter_week), 
            ties.method = "dense"))]

data.table::setattr(
  data_clean$meta_party, 
  "levels",
  c("afd", "gruene", "cdu_csu", "linke", "fdp", "fraktionslos", "spd"))

# data_clean <- data_clean[meta_party != "fraktionslos"]

# EXTRACT TWITTER-SPECIFIC ELEMENTS --------------------------------------------

# String pattern of emojis, hashtags and tags

# test <- data.table::copy(data_clean)

# pattern_emoji <- stringr::str_c(c(
#   "[^\001-\177]", # unicode emojis
#   "(\\:(\\-)?\\))", # simple happy smiley w/ or w/o nose
#   "(\\:(\\-)?\\()", # simple sad smiley w/ or w/o nose
#   "(\\;(\\-)?\\))", # simple winking smiley w/ or w/o nose
#   "\\:P"), # simple smiley sticking tongue out
#   collapse = "|")

emojis_unicode <- data.table::fread(
  here("2_code/1_data/1_training_data", "emojis_unicode.csv"),
  encoding = "UTF-8",
  sep = ";")

pattern_emoji <- stringr::str_c(emojis_unicode$symbol, collapse = "|")

pattern_hashtag <- "(#)[[:alnum:]]+"
pattern_tag <- "@\\S+"

# Extract emojis, hashtags and tags

data_clean[, `:=` (
  twitter_emojis = stringr::str_extract_all(
    twitter_full_text, 
    pattern_emoji), 
  twitter_hashtags = stringr::str_extract_all(
    twitter_full_text, 
    pattern_hashtag),
  twitter_tags = stringr::str_extract_all(
    twitter_full_text, 
    pattern_tag))]

# Split camel case used in hashtags (only in hashtags, only if at most one 
# lowercase letter follows, to escape cases such as "#AfD")
# TODO check if this can be done faster / more elegantly

data_clean[, twitter_full_text := lapply(
  .I, function(i) {
    pattern_camelcase_hashtag <- "(#)(.)+([[:upper:]])([[:lower:]])+"
    pattern_split_camelcase <- "(?<=[[:lower:]])(?=[[:upper:]])"
    components <- unlist(stringr::str_split(twitter_full_text[i], " "))
    case_numbers <- which(
      stringr::str_detect(components, pattern_camelcase_hashtag))
    cases <- components[case_numbers]
    solved_cases <- sapply(
      stringr::str_split(cases, pattern_split_camelcase),
      function(j) paste0(c(j), collapse = " "))
    components[case_numbers] <- solved_cases
    paste0(c(components), collapse = " ")})]  

# Remove emojis, hashtag symbols (not contents) and tags

pattern_unicode <- stringr::str_c(c("[^\001-\177]", collapse = "|"))

patterns_to_remove <- stringr::str_c(
  c(pattern_emoji, "#", pattern_tag, pattern_unicode),
  collapse = "|")

data_clean[
  , twitter_full_text := stringr::str_remove_all(
    twitter_full_text, 
    patterns_to_remove)
  ][, twitter_full_text := stringr::str_squish(twitter_full_text)]

# CREATE UNIQUE DOC ID ---------------------------------------------------------

# Add unique doc_id (only now, since in the beginning, documents may be 
# discarded during language detection etc.)

data_clean[
  , twitter_full_text := as.character(twitter_full_text)
  ][, rank_timestamp := seq_len(.N),
    by = .(twitter_username, twitter_created_at)
    ][, doc_id := paste(
      twitter_username,
      as.character(as.numeric(as.POSIXct(twitter_created_at))),
      rank_timestamp,
      sep = ""),
      by = seq_len(nrow(data_clean))
      ][, rank_timestamp := NULL]

data.table::setkey(data_clean, doc_id)
stopifnot(nrow(data_clean) - length(unique(data_clean$doc_id)) == 0)

cols_to_keep <- c(
  "doc_id",
  names(data_clean)[startsWith(names(data_clean), "twitter_")],
  "label", 
  "topic")

data_labeled_processed <- data_clean[label != "none"][, ..cols_to_keep]
data_unlabeled_processed <- data_clean[label == "none"][, ..cols_to_keep]

data.table::fwrite(
  data_labeled_processed,
  here("2_code/1_data/1_training_data", "data_labeled_processed.csv"),
  sep = ";")

data.table::fwrite(
  data_unlabeled_processed,
  here("2_code/1_data/1_training_data", "data_unlabeled_processed.csv"),
  sep = ";")

data_clean[, topic := NULL]
save_rdata_files(data_clean, folder = "2_code/1_data/2_tmp_data")

# CREATE CORPUS OBJECT ---------------------------------------------------------

# Convert to corpus object

tweets_corpus <- quanteda::corpus(
  x = data_clean,
  docid_field = "doc_id",
  text_field = "twitter_full_text")

save_rdata_files(tweets_corpus, folder = "2_code/1_data/2_tmp_data")
