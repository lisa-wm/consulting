# ------------------------------------------------------------------------------
# READING DATA
# ------------------------------------------------------------------------------

# PURPOSE: read tweet data and discard non-german tweets

get_data <- function(path) {
  
  data <- data.table::fread(
    path, 
    encoding = "UTF-8",
    sep = ",")

  # TODO Make language detection better
  
  data <- data[
    cld3::detect_language(full_text) == "de"
    ][, doc_id := .I]

}


