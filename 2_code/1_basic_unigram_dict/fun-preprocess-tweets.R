# ------------------------------------------------------------------------------
# PRE-PROCESSING TWEETS
# ------------------------------------------------------------------------------

# Purpose: pre-process data for NLP analyses

# TOP-LEVEL FUNCTION -----------------------------------------------------------

preprocess_tweets <- function(data) {
  
  a <- data %>%
    mutate_if(is.character, remove_umlauts) %>% 
    mutate_if(is.character, remove_symbols) %>% 
    mutate(
      hashtags = lapply(hashtags, convert_array_to_list),
      mentions = lapply(mentions, convert_array_to_list),
      # mentions_is_mdb = lapply(
      #   mentions, 
      #   function(x) ifelse(x %in% unique(data$username), 1, 0)),
      created_at = as.POSIXct(
        created_at, 
        format = "%Y-%m-%d %H:%M:%S", 
        tz = "UTC")
    )
  
}

my_text <- c("Alice loves ice cream",
             "Bob hates pickles",
             "Colin is furious")
my_tokens <- tokens(my_text, remove_punct = TRUE)

# HELPER FUNCTIONS -------------------------------------------------------------

# Remove umlauts, symbols etc.

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

remove_symbols <- function(text) {
  
  text %>% 
    str_replace_all(c(
      "\\n" = " ",
      "%" = " Prozent"
      # ,
      # "U+20AC" = " Euro",
      # "U+0024" = " Dollar"
      )) %>% 
    str_remove_all("&amp;|&lt;|&gt;") %>% 
    str_remove_all(" https([^ ]*)")
  
}

# Convert Python arrays to R lists

convert_array_to_list <- function(x) {
  
   x %>% 
    str_match("(?<=\\[).*?(?=\\])") %>% 
    str_replace_all("'", "") %>% 
    str_split(", ")
  
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
