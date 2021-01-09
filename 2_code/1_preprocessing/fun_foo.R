# ------------------------------------------------------------------------------
# UMLAUTS REMOVAL
# ------------------------------------------------------------------------------

# PURPOSE: transform German umlauts and ligature s

foo <- function(text) {
  
  checkmate::assert_character(text)
  
  tolower(text)
  
}
