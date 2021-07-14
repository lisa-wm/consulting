# ------------------------------------------------------------------------------
# UMLAUTS REMOVAL
# ------------------------------------------------------------------------------

# Purpose: transform German umlauts and ligature s

remove_umlauts <- function(text) {
  
  checkmate::assert_character(text)

  # This is necessary for R to convert all representations of umlauts (of which 
  # there are several) to a single one that can then be reliably detected 
  
  text <- stringi::stri_trans_general(text, "Any-Latin")
  
  stringr::str_replace_all(
    text, 
    c(
      "\u00c4" = "Ae",
      "\u00e4" = "ae",
      "\u00d6" = "Oe",
      "\u00f6" = "oe",
      "\u00dc" = "Ue",
      "\u00fc" = "ue",
      "\u00df" = "ss"))

}
