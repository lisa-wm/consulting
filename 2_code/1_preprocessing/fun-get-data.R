# ------------------------------------------------------------------------------
# READING DATA
# ------------------------------------------------------------------------------

# Purpose: read data with correct encoding, relevant columns etc.

# TODO: Remove old part when not needed anymore

get_data <- function(path, is_old_version = FALSE) {
  
  if (is_old_version) {
    
    data <- data.table::fread(
      path, 
      encoding = "UTF-8",
      sep = ",",
      drop = "quoted_status") %>%
      mutate(full_text = ifelse(
        is_retweet == 1, 
        retweet_full_text, 
        full_text)) %>% 
      select(-retweet_full_text)
    
  } else {
    
    data <- data.table::fread(
      path, 
      encoding = "UTF-8",
      sep = ",")
    
  }
  
  # TODO Make language detection better
  
  data <- data[
    cld3::detect_language(full_text) == "de"
    ][, doc_id := .I]

}


