# ------------------------------------------------------------------------------
# PRE-PROCESSING TWEETS
# ------------------------------------------------------------------------------

# IN: raw twitter data + meta data
# OUT: single data file of cleaned tweets and meta data

# READ, CLEAN AND MERGE DATA ---------------------------------------------------

# Read tweets

tweets_raw <- data.table::fread(
  here("1_scraping/output", "tweepy_df_subset_no_retweets.csv"), 
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

# Read meta data

meta_mp_level <- data.table::fread(
  here("1_scraping/output", "abg_twitter_df.csv"),
  encoding = "UTF-8",
  sep = ",",
  drop = "twitter")

meta_mp_level[, `:=` (
  name_matching = remove_noisy_symbols(remove_umlauts(name_matching)),
  bundesland = remove_noisy_symbols(remove_umlauts(bundesland)),
  wahlkreis = remove_noisy_symbols(remove_umlauts(bundesland)))]

# FIXME find out why for so many MP no wahlkreis is scraped

meta_socio_electoral <- data.table::fread(
  here("1_scraping/output", "socioeconomics_zweitstimmen_df.csv"),
  encoding = "UTF-8",
  sep = ",",
  drop = c("district", "wahlkreis")
)

meta_socio_electoral[, bundesland := remove_umlauts(bundesland)]

# Merge tweets and meta data

data_clean <- merge_tweets_meta(
  tweets_data = tweets_raw,
  mp_data = meta_mp_level,
  se_data = meta_socio_electoral
)

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

# CREATE CORPUS OBJECT ---------------------------------------------------------

# Re-convert full text to character (from list)

data_clean[, full_text := as.character(full_text)]

# Add unique doc_id (only now, since in the beginning, documents may be 
# discarded during language detection etc.)
# TODO check whether this can be converted to username_time (time being numeric 
# conversion from posixct)

data_clean[
  , doc_id := .I]

data_clean[
  , rank_timestamp := seq_len(.N),
  by = .(username, created_at)
  ][, doc_id_new := paste(
    username,
    as.character(as.numeric(as.POSIXct(created_at))),
    rank_timestamp,
    sep = ""),
    by = seq_len(nrow(data_clean))]

stopifnot(nrow(data_clean) - length(unique(data_clean$doc_id_new)) == 0)

# Save

save_rdata_files(data_clean, "2_code/attic")

# Convert to corpus object

tweets_corpus <- quanteda::corpus(
  x = data_clean,
  docid_field = "doc_id",
  text_field = "full_text")

save_rdata_files(
  robject = tweets_corpus, 
  folder = "2_code")

# EXTRACT SOME DESCRIPTIVE STATISTICS ------------------------------------------

ndoc(tweets_corpus)

party_colors <- c(
  "deepskyblue",
  "chartreuse4",
  "black",
  "deeppink3",
  "darkgoldenrod1",
  "red"
)

# Tweets over time by party

docvars(tweets_corpus) %>% 
  group_by(time_index_month, party) %>% 
  ggplot(aes(x = year, fill = party)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = party_colors)

# Tweets over time

docvars(tweets_corpus) %>% 
  group_by(year, month) %>% 
  ggplot(aes(x = paste0(year, month))) +
  geom_bar() +
  xlab("time") +
  theme(axis.text.x = element_text(angle = 90))

# Most active users by party

docvars(tweets_corpus) %>%
  group_by(party, username) %>% 
  count() %>% 
  arrange(party, desc(n)) %>% 
  group_by(party) %>%
  top_n(3L) %>% 
  ggplot(aes(x = paste0(party, "_", username), y = n, fill = party)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = party_colors) +
  xlab("party, user") +
  theme(axis.text.x = element_text(angle = 90))

# Tweet length by party

docvars(tweets_corpus) %>% 
  group_by(party) %>% 
  ggplot(aes(x = party, y = word_count, fill = party)) +
  geom_boxplot() +
  scale_fill_manual(values = party_colors)

# Number of used hashtags by party

docvars(tweets_corpus) %>%
  group_by(row_number()) %>% 
  mutate(n_hashtags = length(unlist(hashtags))) %>% 
  group_by(party) %>% 
  ggplot(aes(x = party, y = n_hashtags, fill = party)) +
  geom_boxplot() +
  scale_fill_manual(values = party_colors)

docvars(tweets_corpus) %>%
  group_by(row_number()) %>% 
  mutate(n_hashtags = length(unlist(hashtags))) %>% 
  filter(n_hashtags > 10L) %>% 
  select(username, hashtags) %>% 
  as.data.table() # looks fine

# Number of used emojis

docvars(tweets_corpus) %>%
  group_by(row_number()) %>% 
  mutate(n_emojis = length(unlist(emojis))) %>%
  filter(n_emojis < 10L) %>% 
  ggplot(aes(x = n_emojis)) +
  geom_bar()

# Number of used tags

docvars(tweets_corpus) %>%
  group_by(row_number()) %>% 
  mutate(n_tags = length(unlist(tags))) %>%
  filter(n_tags < 10L) %>% 
  ggplot(aes(x = n_tags)) +
  geom_bar()

# Number of likes

docvars(tweets_corpus) %>%
  filter(favorite_count < 300L) %>% 
  ggplot(aes(x = favorite_count)) +
  geom_histogram(binwidth = 1L)  

# Number of retweets

docvars(tweets_corpus) %>%
  filter(retweet_count < 50L) %>%
  ggplot(aes(x = retweet_count)) +
  geom_histogram(binwidth = 1L)  
