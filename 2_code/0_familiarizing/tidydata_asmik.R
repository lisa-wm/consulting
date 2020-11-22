library(tidytext)
library(stringr)
library(lubridate)
library(wordcloud2)
library(htmlTable)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./20201013_initial_data_exploration.R")
# Function Lisa: 
remove_unwanted_shit = function(text) {
  
  text %>% 
    
    stringi::stri_replace_all_fixed(
      
      c("ä", "ö", "ü", "Ä", "Ö", "Ü", "ß"),
      c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss"),
      vectorize_all = FALSE
      
    ) %>% 
    
    str_replace_all("\\n", " ") %>% 
    
    str_remove_all("https([^ ]*)")
  
}

# TIDY DATA ---------------------------------------------------------------

names(tweepy_df)[15] <- "bundesland"

data <- tweepy_df %>% 
  select(username, full_text, created_at, bundesland, is_retweet) %>% 
  mutate(year = format(created_at, format="%Y")) %>% 
  filter(!str_detect(full_text, "^RT"),
         is_retweet == 0)  #remove tweets from this dataset that are retweets 
data <- na.omit(data)

data_small <- data %>% filter(username %in% c("A_Gloeckner", "BerndWestphal4", "berndsiebert", "stephpilsinger"))

# Activity of different (sampled) users over years (2009-2020)
ggplot(data_small, aes(x = year, fill = username)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE, stat="count") +
  facet_wrap(~username, ncol = 2, nrow = 2) + 
  ggtitle("Activity of different (sampled) users over years (2009-2020)")
ggsave("selected_users.jpg")

# Number of tweets by location (Bundesland)
data %>% 
  select(bundesland) %>% 
  group_by(bundesland) %>% 
  count(bundesland, sort = T) %>%
  filter(n>500) %>% 
  ungroup() %>% 
  mutate(bundesland = reorder(bundesland, n)) %>% 
  ggplot(aes(bundesland, n)) +
  geom_col() +
  coord_flip() +
  labs(x = "User location", y = "Number of tweets", title = "Number of tweets by location")
ggsave("location.jpg")

# Tokenise the tweets and remove the stopwords

# stop_german <- data.frame(word = stopwords::stopwords("de"), stringsAsFactors = FALSE)
# New source: https://countwordsfree.com/stopwords/german
# # Alternative 
# library(lsa) 
# data(stopwords_de)
# stop_german <- data.frame(word = stopwords_de)

library(XML)
linkToXmlFile <- "german_stopwords.xml"
german_stopwords <- xmlParse(linkToXmlFile, encoding="UTF-8")
xmlDataFrame <- xmlToDataFrame(german_stopwords)
stop_german <- data.frame(word = remove_unwanted_shit(xmlDataFrame$text))


tidy_tweets <- data %>% 
  mutate(full_text = str_remove_all(full_text, "&amp;|&lt;|&gt;"),
         full_text = str_replace_all(full_text, "[[:punct:]]", "")) %>% #line cleans out some characters that we don’t want like ampersands and such
  unnest_tokens(word, full_text, token = "tweets") %>% #specialized tokenizer
  filter(!word %in% stop_german$word,
         !word %in% str_remove_all(stop_german$word, "'"), #actually relevant for english apostrophe
         str_detect(word, "[a-z]"))

freq <- tidy_tweets %>% 
  count(word, sort = T) 

# Frequent words 
visual_freq <- freq %>%
  filter(n > 500) %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
visual_freq
ggsave("freqWords_over20.jpg")
# --> maybe other source for german stop words

# library(RTextTools)
# # Zus. Wortstamm bilden 
# freq_stem <- freq %>% 
#   mutate(token_stem = wordStem(.$word, language = "german")) %>% 
#   dplyr::count(token_stem, sort = TRUE) 
# 
# freq_stem %>% 
#   top_n(10) %>% 
#   htmlTable()
# # --> Funktioniert meines Erachtens nicht gut -> die gebildeten Woerter machen nicht viel Sinn


# Wordclouds 
wordcloud <- freq %>%
  top_n(50) %>%
  select(word, n) %>%
  wordcloud2(size = 2.5, color = "darkslategray")
wordcloud



# SENTIMENT ANALYSIS ------------------------------------------------------

neg_df <- read_tsv("./SentiWS_v1.8c_Negative.txt", col_names = FALSE)
names(neg_df) <- c("Wort_POS", "Wert", "Inflektionen")

neg_df %>% 
  mutate(Wort = str_sub(Wort_POS, 1, regexpr("\\|", .$Wort_POS)-1),
         POS = str_sub(Wort_POS, start = regexpr("\\|", .$Wort_POS)+1)) -> neg_df


pos_df <- read_tsv("./SentiWS_v1.8c_Positive.txt", col_names = FALSE)
names(pos_df) <- c("Wort_POS", "Wert", "Inflektionen")

pos_df %>% 
  mutate(Wort = str_sub(Wort_POS, 1, regexpr("\\|", .$Wort_POS)-1),
         POS = str_sub(Wort_POS, start = regexpr("\\|", .$Wort_POS)+1)) -> pos_df

bind_rows("neg" = neg_df, "pos" = pos_df, .id = "neg_pos") -> sentiment_df
sentiment_df %>% select(neg_pos, Wort, Wert, Inflektionen, -Wort_POS) -> sentiment_df

sentiment_df$Wort <- remove_unwanted_shit(sentiment_df$Wort)
sentiment_df$Inflektionen <- remove_unwanted_shit(sentiment_df$Inflektionen)

htmlTable(head(sentiment_df))
names(sentiment_df)[2] <- "word"
names(sentiment_df)[3] <- "score"

# How many pos/neg sentiments: 
tidy_tweets %>% 
  left_join(sentiment_df, by = "word") %>% 
  filter(neg_pos == "neg") %>% 
  summarise(n_distinct_neg = n_distinct(word)) 

tidy_tweets %>% 
  left_join(sentiment_df, by = "word") %>% 
  filter(neg_pos == "pos") %>% 
  summarise(n_distinct_pos = n_distinct(word))
# --> more positive expressions (distinct tokens): 23 vs. 71

tidy_tweets %>% 
  left_join(sentiment_df, by = "word") %>% 
  group_by(neg_pos) %>% 
  filter(!is.na(score)) %>% 
  summarise(Sentimentwert = sum(score)) %>% 
  htmlTable()
# more positive words had been used AND these are much intense than the negative ones


# Tokens mit den extremsten Sentimentwerten: 
tidy_tweets %>% 
  left_join(sentiment_df, by = "word") %>% 
  filter(neg_pos == "pos") %>% 
  distinct(word, .keep_all = TRUE) %>% 
  arrange(-score) %>% 
  filter(row_number() < 11) %>% 
  dplyr::select(word, score) %>% 
  htmlTable::htmlTable(caption = "Tokens with positive extremes")

tidy_tweets %>% 
  left_join(sentiment_df, by = "word") %>% 
  filter(neg_pos == "neg") %>% 
  distinct(word, .keep_all = TRUE) %>% 
  arrange(-score) %>% 
  filter(row_number() < 11) %>% 
  dplyr::select(word, score) %>% 
  htmlTable(caption = "Tokens with negative extremes")

# Visualize on token level: 
# which words contributed the most to the sentiment of electoral discourse

sentiment <- tidy_tweets %>% 
  count(word, sort = T) %>% 
  inner_join(sentiment_df, by = "word") %>% 
  mutate(total_score = n * score)

sentiment %>% 
  top_n(20, abs(total_score)) %>% 
  mutate(word = reorder(word, total_score)) %>% 
  ggplot(aes(word, total_score, fill = total_score > 0)) +
  geom_col(show.legend = F) +
  coord_flip()+
  ggtitle("Tokens' contribution")
ggplot("sent_contribution.jpg")

# How did the sentiment change over time?
sentiment_over_time <- tidy_tweets %>% 
  select(word, created_at) %>% 
  inner_join(sentiment_df, by = "word") %>% 
  mutate(created_at = floor_date(created_at, unit = "1 day")) %>% 
  add_count(created_at, word) %>%
  mutate(total_score = score * n) %>%
  group_by(created_at) 

sentiment_over_time %>% 
  summarise(by_day = sum(total_score)) %>% 
  ggplot(aes(created_at, by_day, fill = by_day > 0)) +
  geom_col(show.legend = F) + 
  ylab("") +
  xlab("net sentiment") +
  scale_y_continuous(labels = scales::comma)

sentiment_over_time %>% 
  summarise(by_day = sum(total_score)/ sum(n)) %>% 
  ggplot(aes(created_at, by_day, fill = by_day > 0)) +
  geom_col(show.legend = F)+ 
  xlab("Net sentiment") +
  ylab("Average sentiment of each word") +
  labs(title = "The average sentiment rating for each word across our data set")
# Warum gibt es "sorgen" sowohl als pos, als auch neg? Wie passiert die Zuordnung?

# # Analysis by Party
# 
# data_small$party <- NA #create a new column for party name
# 
# party_tweets <- data_small %>% 
#   filter(str_detect(full_text, c("afd")) | str_detect(full_text, c("csu"))) #filter the tweets so only ones relating to the party remain
# party_tweets$party <- grepl("afd", party_tweets$full_text) == T #detect whether tweet contains name or not 
# 
# party_tweets$party [party_tweets$party == "TRUE"] <- "afd"
# party_tweets$party [party_tweets$party == "FALSE"] <- "csu"
# 
# party_tweets %>% 
#   ggplot(aes(x = created_at, fill = party)) +
#   geom_histogram() +
#   facet_wrap(~ party, ncol = 1)


# FEATURE EXTRACTION ------------------------------------------------------

# text2vec better option for feature extraction (the normalization step can be done via text2vec easily as well)

library(text2vec)
library(stringi)

data_small$id <- 1:nrow(data_small)


# Preprocessing and Tokenization ------------------------------------------

prep_fun = tolower
tok_fun = word_tokenizer

tokenize
it_train = itoken(stringi::stri_replace_all_regex(data_small$full_text, "[\\p{p}\\p{S}]", ""), # data_small$full_text, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = data_small$id, 
                  progressbar = FALSE)
vocab = create_vocabulary(it_train) # For bigrams: , ngram = c(2L, 2L))

print(vocab)

# remove stopwords
vocab = create_vocabulary(it_train, stopwords = as.character(stop_german$word))

print(vocab)

# filter the input vocabulary and throws out very frequent and very infrequent terms
pruned_vocab = prune_vocabulary(vocab, 
                                term_count_min = 1, 
                                doc_proportion_max = 0.5, 
                                doc_proportion_min = 0.02)


# bow ####


vectorizer = vocab_vectorizer(pruned_vocab)
dtm_train  = create_dtm(it_train, vectorizer)
dtm_train
# view_bow_matrix <- as.matrix(dtm_train)

# tf-idf ####

# define tfidf model
tfidf = TfIdf$new()
# fit model to train data and transform train data with fitted model
dtm_train_tfidf = fit_transform(dtm_train, tfidf)
# view_tfifd_matrix <- as.matrix(dtm_train_tfidf)


# SENTIMENT ANALYSIS ON TWEET-LEVEL ---------------------------------------
# 1. Moeglichkeit (Genaue Funktionsweise noch abzuklaeren)
data$id <- 1:nrow(data)

library(sentimentr)
sentiments_tweets <- sentiment_by(data$full_text)

sentiments_tweets %>% #View()
  ggplot() + geom_density(aes(ave_sentiment))
ggsave("avg_sent_density.jpg")

# 2. Moeglichkeit: (#positive - #negative) / #all [Anzahlen oder Summe der Scores]

satz <- data[1:2,]

data_tokens <- data  %>% 
  mutate(full_text = str_remove_all(full_text, "&amp;|&lt;|&gt;"),
         full_text = str_replace_all(full_text, "[[:punct:]]", "")) %>% #line cleans out some characters that we don’t want like ampersands and such
  unnest_tokens(word, full_text, token = "tweets") %>% #specialized tokenizer
  filter(!word %in% stop_german$word,
         !word %in% str_remove_all(stop_german$word, "'"), #actually relevant for english apostrophe
         str_detect(word, "[a-z]")) %>% 
  left_join(sentiment_df, by = "word") 

table(data_tokens$neg_pos, useNA = "always") / (nrow(data_tokens)) 
# --> ca. 95% der Woerter nicht in Dictionary enthalten

# data_tokens_available <- data_tokens %>% filter(!is.na(neg_pos))
# data_tokens_available %>% 
#   group_by(id,neg_pos) %>% 
#   summarise(
#     n = n(),
#     sum_score = sum(score)) 
#   

# Other scores are as follows:
#   
#  - Negativity or positivity only count the ratio of negative or positive words, respectively. Hence, this value is given by e.g. #negative / #all and is in [0, 1].
#  - Polarity uses the formula (#positive - #negative) / (#positive + #negative).
#  - Ratio is the share of dictionary expressions, i.e. (#positive + #negative) / #all.