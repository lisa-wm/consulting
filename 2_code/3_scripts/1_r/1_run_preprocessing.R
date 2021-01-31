# ------------------------------------------------------------------------------
# PRE-PROCESSING TWEETS
# ------------------------------------------------------------------------------

# IN: raw twitter data + meta data
# OUT: single data file of cleaned tweets and meta data

# READ AND CLEAN DATA ----------------------------------------------------------

# Read tweets
# Attention, these are stored in time-stamped folder to keep downloads from 
# different time points apart --> adjust

tweets_raw <- data.table::fread(
  here("1_scraping/output/202010113_2146", "tweepy_df_subset_no_retweets.csv"), 
  encoding = "UTF-8",
  sep = ",")

tweets_raw <- tweets_raw[created_at >= "2017-09-24"]

# Discard non-German tweets

tweets_raw <- tweets_raw[cld3::detect_language(full_text) == "de"]

# Add word count and date variables

tweets_raw[, `:=` (
  word_count = quanteda::ntoken(full_text, remove_punct = TRUE),
  year = year(created_at),
  month = month(created_at),
  week = week(created_at))]

# Remove umlauts and non-informative symbols
# TODO check whether it's worthwhile to harmonize locations

tweets_raw[
  , full_text := remove_noisy_symbols(remove_umlauts(full_text))
  ][, name_matching := remove_noisy_symbols(remove_umlauts(name_matching))]

save_rdata_files(
  tweets_raw,
  folder = "2_code/1_data/2_tmp_data"
)

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

meta_socio_electoral <- data.table::fread(
  here("1_scraping/output", "socioeconomics_zweitstimmen_df.csv"),
  encoding = "UTF-8",
  sep = ",",
  drop = c("district", "wahlkreis", "bundesland"),
  key = "wahlkreis_nr"
)

# MERGE TWEETS AND META DATA ---------------------------------------------------

# Mind order of join: to perform a left join of dt w/ dt2, use dt2[dt]

# TODO think of sth to handle mps w/ *

data_clean <- meta_mp_level[
  meta_socio_electoral, on = "wahlkreis_nr"
  ][tweets_raw, on = "name_matching"
    ][!stringr::str_detect(party, "\\*")
      ][, `:=` (
      bundesland = as.factor(bundesland),
      party = as.factor(party))
      ][, `:=` (
        time_index_month = frank(list(year, month), ties.method = "dense"),
        time_index_week = frank(list(year, week), ties.method = "dense"))]

data.table::setattr(
  data_clean$party, 
  "levels",
  c("afd", "gruene", "cdu_csu", "linke", "fdp", "fraktionslos", "spd"))

# EXTRACT TWITTER-SPECIFIC ELEMENTS --------------------------------------------

# String pattern of emojis, hashtags and tags

pattern_emoji <- stringr::str_c(c(
  "[^\001-\177]", # unicode emojis
  "(\\:(\\-)?\\))", # simple happy smiley w/ or w/o nose
  "(\\:(\\-)?\\()", # simple sad smiley w/ or w/o nose
  "(\\;(\\-)?\\))", # simple winking smiley w/ or w/o nose
  "\\:P"), # simple smiley sticking tongue out
  collapse = "|")

pattern_hashtag <- "(#)[[:alnum:]]+"
pattern_tag <- "@\\S+"

# Extract emojis, hashtags and tags

data_clean[, `:=` (
  emojis = stringr::str_extract_all(full_text, pattern_emoji), 
  hashtags = stringr::str_extract_all(full_text, pattern_hashtag),
  tags = stringr::str_extract_all(full_text, pattern_tag))]

# Split camel case used in hashtags (only in hashtags, only if at most one 
# lowercase letter follows, to escape cases such as "#AfD")
# TODO check if this can be done faster / more elegantly

data_clean[, full_text := lapply(
  .I, function(i) {
    pattern_camelcase_hashtag <- "(#)(.)+([[:upper:]])([[:lower:]])+"
    pattern_split_camelcase <- "(?<=[[:lower:]])(?=[[:upper:]])"
    components <- unlist(str_split(full_text[i], " "))
    case_numbers <- which(str_detect(components, pattern_camelcase_hashtag))
    cases <- components[case_numbers]
    solved_cases <- sapply(
      str_split(cases, pattern_split_camelcase),
      function(j) paste0(c(j), collapse = " "))
    components[case_numbers] <- solved_cases
    paste0(c(components), collapse = " ")})]  

# Remove emojis, hashtag symbols (not contents) and tags

patterns_to_remove <- stringr::str_c(
  c(pattern_emoji, "#", pattern_tag),
  collapse = "|")

data_clean[
  , full_text := stringr::str_remove_all(full_text, patterns_to_remove)
  ][, full_text := stringr::str_squish(full_text)]

# CREATE UNIQUE DOC ID ---------------------------------------------------------

# Add unique doc_id (only now, since in the beginning, documents may be 
# discarded during language detection etc.)

data_clean[
  , full_text := as.character(full_text)
  ][, rank_timestamp := seq_len(.N),
    by = .(username, created_at)
    ][, doc_id := paste(
      username,
      as.character(as.numeric(as.POSIXct(created_at))),
      rank_timestamp,
      sep = ""),
      by = seq_len(nrow(data_clean))
      ][, rank_timestamp := NULL]

data.table::setkey(data_clean, doc_id)

stopifnot(nrow(data_clean) - length(unique(data_clean$doc_id)) == 0)

save_rdata_files(
  data_clean,
  folder = "2_code/1_data/2_tmp_data"
)

# Save for labeling

# save_rdata_files(data_clean, "2_code/attic")

# CREATE CORPUS OBJECT ---------------------------------------------------------

# Convert to corpus object

tweets_corpus <- quanteda::corpus(
  x = data_clean,
  docid_field = "doc_id",
  text_field = "full_text")

save_rdata_files(
  tweets_corpus, 
  folder = "2_code/1_data/2_tmp_data")

# EXTRACT SOME DESCRIPTIVE STATISTICS ------------------------------------------

# Tweets over time by party

quanteda::docvars(tweets_corpus) %>% 
  group_by(time_index_month, party) %>% 
  ggplot(aes(x = year, fill = party)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = make_party_colors())

# Tweets over time

quanteda::docvars(tweets_corpus) %>% 
  group_by(year, month) %>% 
  ggplot(aes(x = paste0(year, month))) +
  geom_bar() +
  xlab("time") +
  theme(axis.text.x = element_text(angle = 90))

# Most active users by party

quanteda::docvars(tweets_corpus) %>%
  group_by(party, username) %>% 
  count() %>% 
  arrange(party, desc(n)) %>% 
  group_by(party) %>%
  top_n(3L) %>% 
  ggplot(aes(x = paste0(party, "_", username), y = n, fill = party)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = make_party_colors()) +
  xlab("party, user") +
  theme(axis.text.x = element_text(angle = 90))

# Tweet length by party

quanteda::docvars(tweets_corpus) %>% 
  group_by(party) %>% 
  ggplot(aes(x = party, y = word_count, fill = party)) +
  geom_boxplot() +
  scale_fill_manual(values = make_party_colors())

# Number of used hashtags by party

quanteda::docvars(tweets_corpus) %>%
  group_by(row_number()) %>% 
  mutate(n_hashtags = length(unlist(hashtags))) %>% 
  group_by(party) %>% 
  ggplot(aes(x = party, y = n_hashtags, fill = party)) +
  geom_boxplot() +
  scale_fill_manual(values = make_party_colors())

quanteda::docvars(tweets_corpus) %>%
  group_by(row_number()) %>% 
  mutate(n_hashtags = length(unlist(hashtags))) %>% 
  filter(n_hashtags > 10L) %>% 
  select(username, hashtags) %>% 
  as.data.table() # looks fine

# Number of used emojis

quanteda::docvars(tweets_corpus) %>%
  group_by(row_number()) %>% 
  mutate(n_emojis = length(unlist(emojis))) %>%
  filter(n_emojis < 10L) %>% 
  ggplot(aes(x = n_emojis)) +
  geom_bar()

# Number of used tags

quanteda::docvars(tweets_corpus) %>%
  group_by(row_number()) %>% 
  mutate(n_tags = length(unlist(tags))) %>%
  filter(n_tags < 10L) %>% 
  ggplot(aes(x = n_tags)) +
  geom_bar()

# Number of likes

quanteda::docvars(tweets_corpus) %>%
  filter(favorite_count < 300L) %>% 
  ggplot(aes(x = favorite_count)) +
  geom_histogram(binwidth = 1L)  

# Number of retweets

quanteda::docvars(tweets_corpus) %>%
  filter(retweet_count < 50L) %>%
  ggplot(aes(x = retweet_count)) +
  geom_histogram(binwidth = 1L)  