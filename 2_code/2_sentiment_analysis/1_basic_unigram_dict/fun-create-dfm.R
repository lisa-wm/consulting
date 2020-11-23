# ------------------------------------------------------------------------------
# CREATION OF A DOCUMENT-FEATURE MATRIX
# ------------------------------------------------------------------------------

# Purpose: create a document-feature matrix for sentiment analysis

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

tweets_corpus <- corpus(
  tweets_processed,
  docid_field = "doc_id",
  text_field = "full_text"
)

df_mat <- dfm(my_tokens)

cooccurrence_mat <- fcm(my_tokens)
matrix(cooccurrence_mat, ncol = sqrt(length(cooccurrence_mat)))