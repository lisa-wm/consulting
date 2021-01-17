# ------------------------------------------------------------------------------
# CREATION OF SENTIWS DICTIONARY
# ------------------------------------------------------------------------------

# PURPOSE: read SentiWS dictionaries, tidy up, and combine polarities

get_dict_sentiws <- function() {
  
  data_sentiws <- c(
    "SentiWS_v2.0_Positive.txt", 
    "SentiWS_v2.0_Negative.txt")
  
  sapply(
    
    seq_along(data_german_polarity_clues),
    
    function(i) {
      
      # Read data
      
      dict <- data.table::fread(
        here(paste0("2_code/0_external_data/", data_sentiws[i])),
        encoding = "UTF-8",
        col.names = c("lemma", "score", "word"))

      # Extract sentiment words
      
      words <- unlist(str_split(dict$word, ","))
      
      # Remove empty strings and umlauts
      
      words <- words[nchar(words) > 0]
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
