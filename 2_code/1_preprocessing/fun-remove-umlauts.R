# ------------------------------------------------------------------------------
# UMLAUTS REMOVAL
# ------------------------------------------------------------------------------

# PURPOSE: transform German umlauts and ligature s

remove_umlauts <- function(text) {
  
  text %>% 
    stringr::str_replace_all(c(
      "Ä" = "Ae",
      "ä" = "ae",
      "Ö" = "Oe",
      "ö" = "oe",
      "Ü" = "Ue",
      "ü" = "ue",
      "ß" = "ss"
    ))
  
}
