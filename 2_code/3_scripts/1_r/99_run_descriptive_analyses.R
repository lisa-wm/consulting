# ------------------------------------------------------------------------------
# DESCRIPTIVE ANALYSES
# ------------------------------------------------------------------------------

# LABELED DATA -----------------------------------------------------------------

# Load data

load_rdata_files(data_clean, folder = "2_code/1_data/2_tmp_data")
data_training <- data_clean[label != "none"]

cols_relevant <- c(
  "doc_id",
  "label",
  "meta_party",
  "meta_bundesland",
  "meta_unemployment_rate",
  "meta_share_pop_migration",
  "twitter_username",
  "twitter_created_at",
  "twitter_retweet_count")

data_training <- data_training[, ..cols_relevant]

length(unique(data_training$twitter_username))
summary(data_training$twitter_created_at)

# Plot party distribution

plot_party <- function(data, dataset_name) {
  
  data$meta_party <- factor(
    data$meta_party, 
    levels = c(
      "linke", 
      "gruene", 
      "spd", 
      "cdu_csu", 
      "fdp", 
      "afd", 
      "fraktionslos"))
  
  ggplot2::ggplot(
    data[!is.na(meta_party)], 
    ggplot2::aes(x = meta_party, y = ..prop.., group = 1L)) +
    ggplot2::geom_bar(fill = "lightgray") +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_discrete(labels = c(
      "Left",
      "Greens",
      "SPD",
      "CDU/CSU",
      "FDP",
      "AfD",
      "none")) +
    # ggplot2::theme(axis.text.x = element_text(angle = 45L, hjust = 1L)) +
    ggplot2::ylim(c(0L, 0.25)) +
    ggplot2::xlab("party") +
    ggplot2::ylab("number of observations") + 
    ggplot2::ggtitle(dataset_name)
  
}

ggplot2::ggsave(
  here::here("4_report/figures", "obs_per_party.png"),
  gridExtra::grid.arrange(
    plot_party(data_training, "labeled data"),
    plot_party(data_clean, "all data"),
    ncol = 2L),
  height = 2.5,
  width = 10L)

# EXTRACT SOME DESCRIPTIVE STATISTICS ------------------------------------------

load_rdata_files(tweets_corpus, folder = "2_code/1_data/2_tmp_data")

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