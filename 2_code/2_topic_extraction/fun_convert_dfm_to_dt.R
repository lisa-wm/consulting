# ------------------------------------------------------------------------------
# CONVERTING QUANTEDA DFM OBJECTS TO DATA TABLES
# ------------------------------------------------------------------------------

# PURPOSE: convert dfm objects to data tables (built-in function supports
# data frames only)

convert_dfm_to_dt <- function(dfm, key) {

  data.table::setDT(
    quanteda::convert(dfm, to = "data.frame"),
    key = key)
  
}