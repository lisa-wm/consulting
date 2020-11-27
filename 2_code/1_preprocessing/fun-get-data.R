# ------------------------------------------------------------------------------
# READING DATA
# ------------------------------------------------------------------------------

# Purpose: read data with correct encoding, relevant columns etc.

get_data <- function(path) {
  
  data <- data.table::fread(
    path, 
    encoding = "UTF-8",
    sep = ",",
    drop = "quoted_status")
  
  # TODO Substitute "full_text" by "retweet_full_text" for retweets
  
  data <- data %>% 
    mutate(full_text = ifelse(
      is_retweet == 1, 
      retweet_full_text, 
      full_text)) %>% 
    select(-retweet_full_text)
  
  # FIXME Make language detection better
  
  data <- data %>% 
    filter(cld3::detect_language(full_text) == "de")
  
  data <- data %>% 
    mutate(doc_id = row_number())
  
}


