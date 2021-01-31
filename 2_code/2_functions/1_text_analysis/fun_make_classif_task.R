# ------------------------------------------------------------------------------
# CREATION OF CLASSIFICATION TASK
# ------------------------------------------------------------------------------

# PURPOSE: create classfication task to perform sentiment analysis in mlr3

make_classification_task <- function(task_name,
                                     data,
                                     feature_columns,
                                     target_column) {
  
  # Input checks and harmonization
  
  checkmate::assert_string(task_name)
  checkmate::assert_list(feature_columns)
  checkmate::assert_character(unlist(feature_columns))
  checkmate::assert_string(target_column)
  
  mlr3::TaskClassif$new(
    task_name, 
    backend = make_data_backend(
      data = data,
      feature_columns = feature_columns,
      target_column = target_column),
    target = target_column)
  
}

make_data_backend <- function(data, feature_columns, target_column) {
  
  if (!test_data_table(data)) data <- data.table(data)
  dt <- data.table::copy(data)
  
  # Remove unnecessary columns
  
  dt <- dt[, c(unlist(feature_columns), target_column), with = FALSE]
  
  # If necessary, convert target column to factor
  
  if (!checkmate::test_factor(dt[, c(target_column)])) {
    dt[, c(target_column) := factor(get(target_column))]
  }
  
  dt
  
}
