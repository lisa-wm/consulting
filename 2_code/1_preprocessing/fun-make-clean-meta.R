# ------------------------------------------------------------------------------
# PRE-PROCESSING TWITTER METADATA
# ------------------------------------------------------------------------------

# Purpose: pre-process metadata

# HELPER FUNCTIONS -------------------------------------------------------------

# Convert Python arrays to lists

convert_array_to_list <- function(x) {
  
  x %>% 
    stringr::str_match("(?<=\\[).*?(?=\\])") %>% 
    stringr::str_replace_all("'", "") %>% 
    stringr::str_split(", ")
  
}

# TOP-LEVEL FUNCTION -----------------------------------------------------------

make_clean_meta <- function(data, list_columns, date_columns) {

  # Input checks & copy of data to avoid modification by reference
  
  checkmate::assert_data_table(data)
  checkmate::assert_list(list_columns)
  checkmate::assert_list(date_columns)
  
  if (any(!(unlist(c(list_columns, date_columns)) %in% names(data)))) {
    stop("columns could not be found in data")
  }
  
  dt <- data.table::copy(data)
  
  # Converting Python arrays to list, date columns to POSIXct
  # .SD means operations are applied to a subset of columns only
  
  dt[, unlist(list_columns) := lapply(.SD, convert_array_to_list),
     .SDcols = unlist(list_columns)
     ][, unlist(date_columns) := lapply(.SD, as.POSIXct, tz = "UTC"),
       .SDcols = unlist(date_columns)]

}


