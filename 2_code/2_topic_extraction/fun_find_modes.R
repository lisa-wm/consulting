# ------------------------------------------------------------------------------
# FINDING MULTIPLE MINIMA/MAXIMA
# ------------------------------------------------------------------------------

# PURPOSE: extend which.min/which.max to finding multiple entries

find_modes <- function(x, highest = TRUE) {
  
  if (sum(x) == 0) {
    return(NA)
  } else if (highest) {
    result <- which(x == max(x))
  } else {
    result <- which(x == min(x))
    }
  
  result
  
}
