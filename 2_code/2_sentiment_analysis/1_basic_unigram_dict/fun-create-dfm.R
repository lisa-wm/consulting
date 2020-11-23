# ------------------------------------------------------------------------------
# CREATION OF A DOCUMENT-FEATURE MATRIX
# ------------------------------------------------------------------------------

# Purpose: create a document-feature matrix for sentiment analysis

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

create_dfm <- function(corpus, stop_words) {
  
  dfm(
    corpus, 
    remove = stop_words, 
    remove_punct = TRUE, 
    remove_numbers = TRUE,
    verbose = FALSE)
  
}

# df_mat <- dfm(my_tokens)
# 
# cooccurrence_mat <- fcm(my_tokens)
# matrix(cooccurrence_mat, ncol = sqrt(length(cooccurrence_mat)))