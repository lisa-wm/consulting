library(readr)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(quanteda)

data_labeled <- read_delim("D:/UNI/01_Master/3. Semester/Consulting/Tutorial/Themen/03_2_Visualisierung_Evaluierung/data_labeled_processed.csv", 
                                                     ";", escape_double = FALSE, trim_ws = TRUE)

# "topic", "label"

# Take a look at all existing topic categories 
unique(data_labeled$topic)

# Barplots ----------------------------------------------------------------

# Distribution of Labels grouped by Topics over Years

data_labeled$year <- format(data_labeled$twitter_created_at, format = "%Y")

labels_per_year <- data_labeled %>% 
  group_by(year, topic) %>% 
  count(label) 

labels_per_year_filtered <- labels_per_year %>% 
  filter(topic %in% c("Coronamassnahmen", "Medien", "Migrantenkriminalitaet", "Klimapolitik"))

barplot_label_per_year <- ggplot(labels_per_year_filtered, aes(y = n, x = label, fill = topic)) +
  geom_bar(position = "stack",
           stat = "identity") +
  facet_grid(. ~ year) +
  ylab("Count") +
  xlab("Label") +
  theme_classic() +
  theme(text = element_text(size = 17),
        axis.text.x = element_text(angle = 90, size = 17, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(
    "Topic",
    values = c("darkorange3", "cadetblue3", "goldenrod3", "hotpink3")
  ) +
  labs(title = "") +
  ggtitle("Distribution of Labels grouped by Topics over Years")

png("labels_years.png", width = 600, height = 600)
barplot_label_per_year
dev.off()


# Distribution of Labels grouped by Topics

labels_per_topic <- data_labeled %>% 
  group_by(topic) %>% 
  count(label) 

labels_per_topic_filtered <- labels_per_topic %>% 
  filter(topic %in% c("Antisemitismus", "AfD", "Coronamassnahmen",
                      "Parteipolitik", "Coronapolitik", "Klimapolitik",
                      "Rechtsextremismus", "Medien", "Gleichberechtigung"))


barplot_label_per_topic <- ggplot(labels_per_topic_filtered, aes(y = n, x = topic, fill = label)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  ylab("Count") +
  xlab("Topics") +
  theme_classic() +
  theme(text = element_text(size = 17),
        axis.text.x = element_text(angle = 90, size = 17, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(
    "Label",
    values = c("brown2", "darkolivegreen4")
  ) +
  labs(title = "") +
  ggtitle("Distribution of Labels grouped by Topics")

png("labels_per_topic.png", width = 600, height = 600)
barplot_label_per_topic
dev.off()

# Wordclouds --------------------------------------------------------------

# With wordcloud package

word_frequencies <- data_labeled %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, twitter_full_text) %>%
  anti_join(get_stopwords(language = "de")) %>% 
  group_by(topic, label) %>% 
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>% 
  ungroup()

topic_name <- "Coronamassnahmen"
word_frequencies_filtered_topic <- word_frequencies %>% 
  filter(topic == topic_name) %>% 
  top_n(20) %>% 
  mutate(color = ifelse(label == "positive", "darkolivegreen4", "brown2"))

# Frequent words 
visualize_word_frequencies <- word_frequencies_filtered_topic %>%
  mutate(word = reorder(word, n)) %>%
  filter(n > 2) %>% 
  ggplot(aes(reorder(word, -n), n, fill = label)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ylab("Count") +
  xlab("Words") +
  theme_classic() +
  theme(text = element_text(size = 17),
        axis.text.x = element_text(size = 17, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(
    "Label",
    values = c("brown2", "darkolivegreen4")
  ) +
  labs(title = "") +
  ggtitle(paste0("Word frequencies in topic ", topic_name))

png("word_frequencies.png", width = 600, height = 600)
visualize_word_frequencies
dev.off()

set.seed(1234)

png("wordcloud_tidytext.png", width = 600, height = 600)
wordcloud(words = word_frequencies_filtered_topic$word, 
          freq = word_frequencies_filtered_topic$n, 
          min.freq = 1,
          max.words = 200, 
          ordered.colors = TRUE,
          random.order = FALSE,
          scale = c(3, 0.5),
          rot.per = 0.1,
          colors = word_frequencies_filtered_topic$color)
legend("topright", legend = levels(factor(word_frequencies_filtered_topic$label)), 	
       text.col = c("brown2", "darkolivegreen4"))
dev.off()


# With quanteda package

dfm_filtered_topic <- dfm(
  corpus_subset(
    corpus(data_labeled,
           text_field = "twitter_full_text"),
    topic %in% c("AfD")
  ),
  remove = stopwords("de"),
  remove_punct = TRUE,
  groups = "label"
) %>%
  dfm_trim(min_termfreq = 3)

png("wordcloud_quanteda.png", width = 600, height = 600)
textplot_wordcloud(dfm_filtered_topic, 
                   comparison = TRUE, 
                   max_words = 300,
                   adjust = 0.1,
                   color = c("brown2", "darkolivegreen4"))
dev.off()

# Feature Extraction ------------------------------------------------------
corpus <- c("Die Ausgrenzung von MigrantInnen ist inakzeptabel und rassistisch.",
            "Die Maskenpflicht ist sinnvoll.",
            "Die Diskriminierung von Frauen ist inakzeptabel.")

# BOW
toks_corpus <- tokens(corpus, remove_punct = TRUE)
dfmat_corpus <- dfm(toks_corpus)
print(dfmat_corpus)

# TF-IDF

tfidf_corpus <- dfm_tfidf(dfmat_corpus, scheme_tf = "count") %>% round(digits = 2)

# co occurence glove

fcm(toks_corpus, context = "window", window = 2)