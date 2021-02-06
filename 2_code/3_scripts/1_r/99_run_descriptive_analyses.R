# ------------------------------------------------------------------------------
# DESCRIPTIVE ANALYSES
# ------------------------------------------------------------------------------

# IN: corpus object of cleaned tweets and meta data
# OUT: -

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