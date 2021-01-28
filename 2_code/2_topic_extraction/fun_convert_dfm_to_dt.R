# ------------------------------------------------------------------------------
# CONVERTING QUANTEDA OBJECTS TO DATA TABLES
# ------------------------------------------------------------------------------

# PURPOSE: convert quanteda objects to data tables (built-in function supports
# data frames only)

convert_qtda_to_dt <- function(quanteda_object, key) {

  data.table::setDT(
    quanteda::convert(dfm, to = "data.frame"),
    key = key)
  
}
