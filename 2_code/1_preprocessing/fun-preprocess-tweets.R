# ------------------------------------------------------------------------------
# PRE-PROCESSING TWEETS
# ------------------------------------------------------------------------------

# Purpose: pre-process data for NLP analyses

# HELPER FUNCTIONS -------------------------------------------------------------

# Remove umlauts

remove_umlauts <- function(text) {
  
  text %>% 
    stringr::str_replace_all(c(
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
# TODO Check if everything is covered, then cut redundancies

remove_symbols <- function(text) {
  
  text %>% 
    stringr::str_replace_all(c("\\n" = " ")) %>% 
    stringr::str_remove_all(str_c(c(
      "\U0022", 
      "\U0027", 
      "\U2018", 
      "\U2019", 
      "\U201C", 
      "\U201D", 
      "\U201E", 
      "\U201F"), 
      collapse = "|")) %>% # all kinds of quotes
    stringr::str_remove_all("&amp;|&lt;|&gt;") %>% # ampersands etc.
    stringr::str_remove_all(" http([^ ]*)") %>% # hyperlinks
    stringr::str_remove_all("#") %>% # hashtag symbols
    stringr::str_remove_all("%") # percent signs
  
}


# FIXME Extracting mentions works only for first tag

remove_mentions <- function(text) {
  
  stringr::str_remove_all(text, "^@(.*?)+(?=\\s)")
  
}

# TOP-LEVEL FUNCTION -----------------------------------------------------------

# Remove all umlauts and symbols irrelevant to sentiment analysis,
# extract emojis

# FIXME Make emoji extraction better

pattern_emoji <- stringr::str_c(c(
  "[^\001-\177]", # unicode emojis
  "(\\:(\\-)?\\))", # simple happy smiley w/ or w/o nose
  "(\\:(\\-)?\\()", # simple sad smiley w/ or w/o nose
  "(\\;(\\-)?\\))"), # simple winking smiley w/ or w/o nose
  collapse = "|")

preprocess_basic <- function(data) {
  
  data %>% 
    mutate_if(is.character, remove_umlauts) %>%
    mutate_if(is.character, remove_symbols) %>% 
    mutate_if(
      is.character, 
      .funs = list(
        emojis = ~ do.call(
          stringr::str_extract_all, 
          list(., pattern_emoji)))) %>% 
    mutate_if(
      is.character,
      ~ do.call(stringr::str_remove_all, list(., pattern_emoji))) 
}

# TESTS ------------------------------------------------------------------------

# TODO Write decent tests

# test_file(here("2_code/1_preprocessing", "test-preprocess-tweets.R"))
