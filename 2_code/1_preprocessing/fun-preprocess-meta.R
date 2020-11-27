# ------------------------------------------------------------------------------
# PRE-PROCESSING TWITTER METADATA
# ------------------------------------------------------------------------------

# Purpose: pre-process metadata

# HELPER FUNCTIONS -------------------------------------------------------------

# Convert Python arrays to lists

convert_array_to_list <- function(x) {
  
  x %>% 
    stringr::str_match("(?<=\\[).*?(?=\\])") %>% 
    stringr::str_replace_all("'", "") %>% 
    stringr::str_split(", ")
  
}

# TOP-LEVEL FUNCTION -----------------------------------------------------------

# FIXME Suppress warning that is displayed every time

preprocess_meta <- function(data) {
  
  mdb_on_twitter <- unique(data$username)
  
  data %>%
    mutate_if(
      stringr::str_detect(., "\\[.*?\\]"), 
      ~ lapply(., convert_array_to_list)) %>% 
    mutate(
      created_at = as.POSIXct(
        created_at,
        format = "%Y-%m-%d %H:%M:%S", 
        tz = "UTC")
      # ,
      # mentions_is_mdb = list(sapply(
      #   unlist(mentions),
      #   function(x) ifelse(x %in% mdb_on_twitter, 1, 0)))
      )
  
}

