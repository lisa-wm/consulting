# ------------------------------------------------------------------------------
# CREATION OF GERMAN POLARITY CLUES DICTIONARY
# ------------------------------------------------------------------------------

# PURPOSE: read German polarity clues dictionaries, tidy up, and combine 
# polarities

get_dict_gpc <- function() {
 
  data_german_polarity_clues <- c(
    "GermanPolarityClues-Positive-21042012.tsv", 
    "GermanPolarityClues-Negative-21042012.tsv",
    "GermanPolarityClues-Neutral-21042012.tsv")
  
  dict_list <- lapply(
    
    seq_along(data_german_polarity_clues),
    
    function(i) {
      
      # Read data
      
      dict <- data.table::fread(
        here("2_code/0_external_data", data_german_polarity_clues[i]),
        encoding = "UTF-8",
        drop = c(2:4, 6),
        header = FALSE,
        col.names = c("term", "polarity_score"),
        quote = "")
      
      # Assign strong polarity if polarity score is available, and remove
      # umlauts
      
      dict[
        , polarity_degree := ifelse(
          str_detect(polarity_score, "[0-9]"),
          "strong",
          "weak"
        )
        ][, term := remove_umlauts(tolower(term))
          ][, term := SnowballC::wordStem(term, language = "de")]
      
      dict <- unique(dict)
      
    })
  
  names(dict_list) <- c("positive", "negative", "neutral")
  
  dict_list
   
}
