# ------------------------------------------------------------------------------
# SYMBOLS REMOVAL
# ------------------------------------------------------------------------------

# PURPOSE: remove symbols that carry no information for topic modeling and/or 
# sentiment analysis

# TODO Check if everything is covered, then cut redundancies

remove_noisy_symbols <- function(text) {
  
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
    stringr::str_remove_all("%") %>% # percent signs
    stringr::str_remove_all(" http([^ ]*)") # hyperlinks
  
}
