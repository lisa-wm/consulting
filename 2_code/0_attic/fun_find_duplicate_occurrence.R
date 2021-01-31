# ------------------------------------------------------------------------------
# CUMULATIVE COUNT OF ELEMENTS IN ARRAY
# ------------------------------------------------------------------------------

# PURPOSE: cum-count occurrences of terms in keyword list so duplicates can be 
# removed where they occur in a lower position only

find_duplicate_occurrence <- function(data) {
  
  data_raw <- c(t(data))
  
  count_cum <- sapply(
    seq_along(data_raw), 
    function(i) sum(data_raw[1:i] == data_raw[i]))
  
  count_cum_dt <- as.data.table(
    matrix(count_cum, ncol = ncol(data), byrow = TRUE))
  setnames(count_cum_dt, names(data))
  
  count_cum_dt > 1
  
}
