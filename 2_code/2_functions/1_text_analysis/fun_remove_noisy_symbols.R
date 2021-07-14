# ------------------------------------------------------------------------------
# SYMBOLS REMOVAL
# ------------------------------------------------------------------------------

# Purpose: remove symbols that carry no information for topic modeling and/or 
# sentiment analysis

remove_noisy_symbols <- function(text) {
  
  checkmate::assert_character(text)
  
  text <- stringr::str_replace_all(text, c("\\n" = " "))
  
  text <- stringr::str_remove_all(
    text,
    stringr::str_c(c(
      "\U0022", 
      "\U0027", 
      "\U2018", 
      "\U2019", 
      "\U201C", 
      "\U201D", 
      "\U201E", 
      "\U201F",
      "&amp;",
      "&lt;", 
      "&gt;",
      "%",
      " http([^ ]*)",
      "http([^ ]*)",
      "\\\n"), 
      collapse = "|"))
  
  stringr::str_squish(text)
  
}
