# ------------------------------------------------------------------------------
# CREATION OF SENTIWS DICTIONARY
# ------------------------------------------------------------------------------

# PURPOSE: read SentiWS dictionaries, tidy up, and combine polarities

get_dict_sentiws <- function() {
  
  data_sentiws <- c(
    "SentiWS_v2.0_Positive.txt", 
    "SentiWS_v2.0_Negative.txt")
  
  dict_list <- lapply(
    
    seq_along(data_sentiws),
    
    function(i) {
      
      # Read data
      
      dict <- data.table::fread(
        here(paste0("2_code/0_external_data/", data_sentiws[i])),
        encoding = "UTF-8",
        drop = 1L,
        col.names = c("polarity_score", "term"))
      
      # Assign strong polarity if polarity score is available, and remove
      # umlauts
      
      median_polarity_score <- quantile(abs(dict$polarity_score), 0.75)
      
      dict[
        , polarity_degree := ifelse(
          abs(polarity_score) > median_polarity_score,
          "strong",
          "weak"
        )
      ][, term := remove_umlauts(tolower(term))
        ][, term := str_split(term, ",")]
      
      dict <- dict[
        , list(term = as.character(unlist(term))), 
        by = list(polarity_score, polarity_degree)
        ][, term := SnowballC::wordStem(term, language = "de")]
      
      dict <- unique(dict[nchar(term) > 0])
      
    })
  
  names(dict_list) <- c("positive", "negative")
  
  dict_list
  
}
