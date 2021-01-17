# ------------------------------------------------------------------------------
# CREATION OF GERMAN POLARITY CLUES DICTIONARY
# ------------------------------------------------------------------------------

# PURPOSE: read German polarity clues dictionaries, tidy up, and combine 
# polarities

get_dict_gpc <- function() {
 
  data_german_polarity_clues <- c(
    "GermanPolarityClues-Positive-21042012.tsv", 
    "GermanPolarityClues-Negative-21042012.tsv")
  
  sapply(
    
    seq_along(data_german_polarity_clues),
    
    function(i) {
      
      # Read data
      
      dict <- data.table::fread(
        here(paste0("2_code/0_external_data/", data_german_polarity_clues[i])),
        encoding = "UTF-8",
        drop = 2:6)
      
      # Remove umlauts

      words <- unlist(dict)
      words <- remove_umlauts(words)
      
      # Save

      polarities <- c("positive", "negative")

      assign(
        paste0("words_", polarities[i]), 
        words,
        pos = parent.env(environment()))
      
    })
  
  list(
    positive = words_positive, 
    negative = words_negative)
   
}
