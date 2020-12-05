# ------------------------------------------------------------------------------
# TASK CREATION FOR ML MODELS
# ------------------------------------------------------------------------------

# Purpose: make mlr3 task for machine learning classifiers

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

make_data_backend <- function(data, target_column) {
  
  # Input checks and harmonization
  
  assert_string(target_column)
  if (!test_data_table(data)) data <- data.table(data)
  dt <- copy(data)

  # If necessary, convert target column to factor
  
  if (!test_factor(dt[, c(target_column)])) {
    dt[, c(target_column) := factor(get(target_column))]
  }

  dt
  
}


