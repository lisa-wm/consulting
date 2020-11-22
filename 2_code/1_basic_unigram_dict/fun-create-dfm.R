df_mat <- dfm(my_tokens)

cooccurrence_mat <- fcm(my_tokens)
matrix(cooccurrence_mat, ncol = sqrt(length(cooccurrence_mat)))