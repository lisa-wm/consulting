# ------------------------------------------------------------------------------
# PRE-PROCESSING TWEETS
# ------------------------------------------------------------------------------

# Purpose: pre-process data for NLP analyses

# TOP-LEVEL FUNCTION -----------------------------------------------------------

preprocess_basic <- function(data) {
  
  text %>% 
    mutate_if(is.character, remove_umlauts) %>% 
    mutate_if(is.character, remove_symbols) %>% 
    mutate_if(is.character, .funs = list(emojis = ~ extract_emojis(.)))
  
}

preprocess_advanced <- function(text) {
  
}

my_text <- c("Alice loves ice cream",
             "Bob hates pickles",
             "Colin is furious")
my_tokens <- tokens(my_text, remove_punct = TRUE)

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
    str_remove_all("#") %>% # hashtag symbols
    str_remove_all("@") # targets in mentions
  
}

# Extract emojis
# !!!! not optimal !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

extract_emojis <- function(text) {
  
  str_extract_all(
    text, 
    str_c(c(
      "[^\001-\177]", # unicode emojis
      "(\\:(\\-)?\\))", # simple happy smiley w/ or w/o nose
      "(\\:(\\-)?\\()"), # simple sad smiley w/ or w/o nose
    collapse = "|"))
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
