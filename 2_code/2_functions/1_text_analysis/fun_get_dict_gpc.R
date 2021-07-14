# ------------------------------------------------------------------------------
# CREATION OF GERMAN POLARITY CLUES DICTIONARY
# ------------------------------------------------------------------------------

# Purpose: read German polarity clues dictionaries, tidy up, and combine 
# polarities

get_dict_gpc <- function() {
 
  data_gpc <- c(
    "GermanPolarityClues-Positive-21042012.tsv", 
    "GermanPolarityClues-Negative-21042012.tsv")
  
  dict_list <- lapply(
    
    seq_along(data_gpc),
    
    function(i) {
      
      # Read data
      
      dict <- data.table::fread(
        here::here("2_code/1_data/0_external_data", data_gpc[i]),
        encoding = "UTF-8",
        drop = c(2L:4L, 6L),
        header = FALSE,
        col.names = c("term", "polarity_score"),
        quote = "")
      
      # Assign strong polarity if polarity score is available, and remove
      # umlauts
      
      dict[
        , polarity_degree := ifelse(
          stringr::str_detect(polarity_score, "[0-9]"),
          "strong",
          "weak")
        ][, term := remove_umlauts(tolower(term))
          ][, term := SnowballC::wordStem(term, language = "de")]
      
      dict <- unique(dict)
      
    })
  
  names(dict_list) <- c("positive", "negative")
  
  dict_list
   
}
