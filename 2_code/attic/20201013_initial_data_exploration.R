# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# INITIAL DATA EXPLORATION 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ------------------------------------------------------------------------------
# PREREQ -----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# WORKING DIRECTORY ------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# PACKAGES ---------------------------------------------------------------------

my_type = ifelse(
  Sys.info()[["sysname"]] == "Linux", 
  "source", 
  "binary"
)

packages_required = c(
  "tidyverse",
  "data.table",
  "lubridate",
  "quanteda"
)

not_installed = packages_required[!packages_required %in% 
                                    installed.packages()[, "Package"]]

if (length(not_installed) > 0) {
  
  lapply(
    not_installed,
    install.packages,
    repos = "http://cran.us.r-project.org",
    dependencies = TRUE,
    type = my_type
  )
  
}

lapply(packages_required, library, character.only = TRUE)

# ------------------------------------------------------------------------------
# DATA PREP --------------------------------------------------------------------
# ------------------------------------------------------------------------------

# IMPORT -----------------------------------------------------------------------

abg_twitter_df = fread("abg_twitter_df.csv", encoding = "UTF-8")
tweepy_df = fread("tweepy_df.csv", encoding = "UTF-8")

# GENERAL PRE-PROCESSING -------------------------------------------------------

# Helper function for converting Python arrays to R lists

convert_array_to_list = function(x) {
  
  inner = function(x) {
    
    if(x == "[]") {return(NA)}
    
    else {
      
      components = unlist(strsplit(x, "'"))
      components = components[-c(1, length(components))]
      components = components[grep(",", components, invert = TRUE)]
      return(components)
      
    }
    
  }
  
  return(lapply(x, inner))
  
}

# Apply to mentions in hashtags, obtained as arrays

tweepy_df = tweepy_df %>% 
  mutate(
    mentions = convert_array_to_list(mentions),
    hashtags = convert_array_to_list(hashtags)
)

# Flag mentions where another MdB is tagged

mdb_on_twitter = unique(tweepy_df$username)

tweepy_df = tweepy_df %>% 
  mutate(
    mentions_is_mdb = lapply(
      mentions, 
      function(x) ifelse(x %in% mdb_on_twitter, 1, 0)
    )
  )

# Convert created_at to date format

tweepy_df = tweepy_df %>% 
  mutate(
    created_at = as.POSIXct(
      created_at, 
      format = "%Y-%m-%d %H:%M:%S", 
      tz = "UTC")
    )

# TEXUTAL PRE-PROCESSING -------------------------------------------------------

# Remove umlauts, newline commands and hyperlinks from text

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

tweepy_df = tweepy_df %>% 
  mutate(
    full_text = remove_unwanted_shit(full_text),
    retweet_full_text = remove_unwanted_shit(retweet_full_text)
  )

# ADDITIONAL VARIABLES ---------------------------------------------------------

# Append columns with MdB info

tweepy_df = left_join(
  tweepy_df,
  abg_twitter_df,
  by = "name_matching"
)

head(tweepy_df)

# ------------------------------------------------------------------------------
# DATA EXPLORATION -------------------------------------------------------------
# ------------------------------------------------------------------------------

# Time frame

tweepy_df %>% 
  summarise(
    from = min(created_at, na.rm = TRUE),
    to = max(created_at, na.rm = TRUE)
  )

# Number of MdB on Twitter per party

tweepy_df %>%
  group_by(party) %>% 
  summarise(n_distinct(username))

# Median number of Tweets per party and person

tweepy_df %>%
  group_by(party, username) %>% 
  summarise(per_person = n()) %>% 
  summarise(med_per_party = median(per_person))

# ------------------------------------------------------------------------------
# NLP BASICS -------------------------------------------------------------------
# ------------------------------------------------------------------------------

test_tweet = tweepy_df$full_text[9]

tokenize_word(test_tweet)

test_tweet_tbl = as_tibble(test_tweet)

test_tweet_tbl %>% 
  as_tibble() %>% 
  tidytext::unnest_tokens(word, value, to_lower = FALSE)



