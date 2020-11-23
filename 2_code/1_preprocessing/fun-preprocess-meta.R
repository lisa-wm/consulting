# ------------------------------------------------------------------------------
# PRE-PROCESSING TWITTER METADATA
# ------------------------------------------------------------------------------

# Purpose: pre-process metadata

# TOP-LEVEL FUNCTION -----------------------------------------------------------

preprocess_meta <- function(data) {
  
  head(data) %>%
    mutate_if(
      str_detect(., "\\[.*?\\]"), 
      ~ lapply(., convert_array_to_list)) %>% 
    mutate(created_at = as.POSIXct(
      created_at, 
      format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  # mutate(
  # mentions_is_mdb = lapply(
  #   mentions,
  #   function(x) ifelse(x %in% unique(data$username), 1, 0)),
      
}

# HELPER FUNCTIONS -------------------------------------------------------------

# Convert Python arrays to lists

convert_array_to_list <- function(x) {
  
  x %>% 
    str_match("(?<=\\[).*?(?=\\])") %>% 
    str_replace_all("'", "") %>% 
    str_split(", ")
  
}
