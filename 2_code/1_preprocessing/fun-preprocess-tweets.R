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
    stringr::str_remove_all("(?=@).*?(?=\\s)") %>% # targets
    stringr::str_remove_all("%") # percent signs
  
}

# TOP-LEVEL FUNCTION -----------------------------------------------------------

# Remove all umlauts and symbols irrelevant to sentiment analysis,
# extract emojis

preprocess_basic <- function(data, column) {
  
  # Input checks & copy of data to avoid modification by reference
  
  assert_data_table(data)
  assert_string(column)

  # TODO Make emoji extraction better
  
  pattern_emoji <- stringr::str_c(c(
    "[^\001-\177]", # unicode emojis
    "(\\:(\\-)?\\))", # simple happy smiley w/ or w/o nose
    "(\\:(\\-)?\\()", # simple sad smiley w/ or w/o nose
    "(\\;(\\-)?\\))"), # simple winking smiley w/ or w/o nose
    collapse = "|")
  
  dt <- copy(data)
  
  # Perform preprocessing

  # (c around column is needed bc it is supplied as character; otherwise
  # data.table will just create a new column named "column")

  dt[, c(column) := remove_umlauts(get(column))
     ][, c(column) := remove_symbols(get(column))
       ][, emojis := stringr::str_extract_all(get(column), pattern_emoji)
         ][, c(column) := stringr::str_remove_all(get(column), pattern_emoji)]
  
}


# TESTS ------------------------------------------------------------------------

# TODO Write decent tests

# test_file(here("2_code/1_preprocessing", "test-preprocess-tweets.R"))
