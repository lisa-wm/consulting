# ------------------------------------------------------------------------------
# DESCRIPTIVE ANALYSES
# ------------------------------------------------------------------------------

# LOAD DATA --------------------------------------------------------------------

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

# MISCELLANEOUS DESCRIPTIVES ---------------------------------------------------

length(unique(data_training$twitter_username))
summary(data_training$twitter_created_at)

set.seed(1L)
knitr::kable(
  data_clean[
    label != "none"
    ][sample(data_clean[label != "none", .I], 5L)
      ][, .(username = twitter_username,
            party = meta_party,
            created_at = twitter_created_at,
            text = twitter_full_text,
            followers_count = twitter_followers_count,
            unemployment_rate = meta_unemployment_rate,
            label)], 
  format = "latex")

# OBS PER PARTY ----------------------------------------------------------------

plot_party <- function(data, dataset_name) {
  
  data_party <- data.table::copy(data)[
    , obs_per_party := .N, by = meta_party
    ][, .(meta_party, obs_per_party)]
  
  data_party <- unique(data_party)[
    , obs_per_party := obs_per_party / sum(obs_per_party)]
  
  #(https://www.bundeswahlleiter.de/info/presse/mitteilungen/bundestagswahl-2017/34_17_endgueltiges_ergebnis.html)
  
  data_official <- data.frame(
    party = c("linke", "gruene", "spd", "cdu_csu", "fdp", "afd", "none"),
    seats = c(69L, 67L, 153L, 246L, 80L, 94L, 0L) / 709L)
  
  data_total <- data_party[data_official, on = c("meta_party" = "party")]
  
  data_long <- data.table::melt(
    data_total,
    id.vars = c("meta_party"),
    measure.vars = c("obs_per_party", "seats"))

  ggplot2::ggplot(
    data_long[meta_party != "none"], 
    ggplot2::aes(x = meta_party, y = value, fill = variable)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_discrete(
      labels = c("AfD", "CDU/CSU", "FDP", "Greens", "Left", "SPD")) +
    ggplot2::scale_fill_manual(
      name = "",
      values = c("deepskyblue2", "lightgray"),
      labels = c("training observations", "seats in 2017 Bundestag")) +
    ggplot2::xlab("party") +
    ggplot2::ylab("relative frequency") + 
    ggplot2::ggtitle(dataset_name)
  
}

ggplot2::ggsave(
  here::here("4_report/figures", "obs_per_party.png"),
  ggpubr::ggarrange(
    plot_party(data_clean[label != "none"], "labeled data"),
    plot_party(data_clean, "all data"),
    ncol = 2L,
    common.legend = TRUE,
    legend = "bottom"),
  height = 2.5,
  width = 8L)

# OBS OVER TIME ----------------------------------------------------------------

plot_time <- ggplot2::ggplot(
  data_training,
  ggplot2::aes(x = as.Date(twitter_created_at))) + 
  ggplot2::geom_histogram(binwidth = 5L, fill = "lightgray") +
  ggplot2::theme_minimal() +
  ggplot2::scale_x_date(
    date_labels = "%m/%Y", 
    date_breaks = "3 months") +
  ggplot2::theme(axis.text.x = element_text(angle = 45L, hjust = 1L)) +
  ggplot2::xlab("time") +
  ggplot2::ylab("number of observations")

ggplot2::ggsave(
  here::here("4_report/figures", "obs_over_time.png"),
  plot_time,
  height = 2.5,
  width = 8L)

# CLASS DISTRIBUTION -----------------------------------------------------------

plot_class <- ggplot2::ggplot(
  data_training[, n_label := .N], 
  ggplot2::aes(x = n_label, fill = label)) +
  ggplot2::geom_bar(position = "fill") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::coord_flip() +
  ggplot2::scale_x_continuous(labels = NULL, breaks = NULL) +
  ggplot2::ylab("share") +
  ggplot2::xlab("") + 
  ggplot2::scale_fill_manual(values = c("deepskyblue2", "lightgray")) +
  ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))

ggplot2::ggsave(
  here::here("4_report/figures", "class_dist.png"),
  plot_class,
  height = 2.2,
  width = 4.5)

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