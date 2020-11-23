# ------------------------------------------------------------------------------
# PRE-PROCESSING TWEETS
# ------------------------------------------------------------------------------

# Purpose: pre-process data for NLP analyses

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

# Remove all umlauts and symbols irrelevant to sentiment analysis

preprocess_basic <- function(data) {
  
  data %>% 
    mutate_if(is.character, remove_umlauts) %>%
    mutate_if(is.character, remove_symbols)
  
}

# Extract emojis, then remove them from text; remove mentions

preprocess_advanced <- function(data) {
  
  data %>% 
    mutate_if(
      is.character, 
      .funs = list(
        emojis = ~ do.call(extract_emojis, list(., pattern_emoji)))) %>% 
    mutate_if(
      is.character,
      ~ do.call(remove_emojis, list(., pattern_emoji))) %>% 
    mutate_if(is.character, remove_mentions)
 
}

# HELPER FUNCTIONS -------------------------------------------------------------

# Remove umlauts

remove_umlauts <- function(text) {
  
  text %>% 
    str_replace_all(c(
      "Ä" = "Ae",
      "ä" = "ae",
      "Ö" = "Oe",
      "ö" = "oe",
      "Ü" = "Ue",
      "ü" = "ue",
      "ß" = "ss"
    ))
  
}

# Remove unwanted symbols that would hamper sentiment analysis

remove_symbols <- function(text) {
  
  text %>% 
    str_replace_all(c(
      "\\n" = " ",
      "%" = " Prozent"
      )) %>% 
    str_remove_all(str_c(c(
      "\U0022", 
      "\U0027", 
      "\U2018", 
      "\U2019", 
      "\U201C", 
      "\U201D", 
      "\U201E", 
      "\U201F"), 
      collapse = "|")) %>% # all kinds of quotes
    str_remove_all("&amp;|&lt;|&gt;") %>% # ampersands etc.
    str_remove_all(" http([^ ]*)") %>% # hyperlinks
    str_remove_all("#") # hashtag symbols

}

# Extract emojis
# !!!! not optimal !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

pattern_emoji <- str_c(c(
  "[^\001-\177]", # unicode emojis
  "(\\:(\\-)?\\))", # simple happy smiley w/ or w/o nose
  "(\\:(\\-)?\\()", # simple sad smiley w/ or w/o nose
  "(\\;(\\-)?\\))"), # simple winking smiley w/ or w/o nose
  collapse = "|")

extract_emojis <- function(text, pattern) {
  str_extract_all(text, pattern)
}

# Remove emojis from text

remove_emojis <- function(text, pattern) {
  str_remove_all(text, pattern)
}

# Remove mentions from text
# !!!! does not work, extracts only first match !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

remove_mentions <- function(text) {
  str_remove_all(text, "^@(.*?)+(?=\\s)")
}

# TESTS ------------------------------------------------------------------------

test_data <- data[1:20, ]

tweepy_df = tweepy_df %>% 
  mutate(
    mentions_is_mdb = lapply(
      mentions, 
      function(x) ifelse(x %in% mdb_on_twitter, 1, 0)
    )
  )

test_file(here("2_code/1_basic_unigram_dict", "test-preprocess-tweets.R"))
