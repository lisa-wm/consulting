# ------------------------------------------------------------------------------
# SPLITTING DATA IN TRAINING AND TEST SETS
# ------------------------------------------------------------------------------

# Purpose: perform train-test split so the "untouched test set" principle is
# adhered to (nested resampling is then performed on the training set only)

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

make_train_test_split <- function(task, ratio) {
  
  set.seed(1)
  training_instances <- sample(task$nrow, ratio * task$nrow)
  test_instances <- setdiff(seq_len(task$nrow), training_instances)
  
  list(
    train = task$clone()$filter(training_instances),
    test = task$clone()$filter(test_instances)
  )

}
