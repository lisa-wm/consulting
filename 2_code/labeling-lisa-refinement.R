summary(tweepy_subset_labeling_lisa$favorite_count)
summary(tweepy_subset_labeling_lisa$word_count)
summary(tweepy_subset_labeling_lisa$followers_count)

small <- tweepy_subset_labeling_lisa[
  favorite_count > 7 & word_count >= 38 & created_at > "2020-01-01"
  & followers_count > 15000 & length(unlist(emojis)) > 0]

small_2 <- tweepy_subset_labeling_lisa[
  favorite_count > 5 & word_count >= 38 & created_at > "2020-01-01"
  & followers_count > 12000 & length(unlist(emojis)) > 0]

new_rows <- setdiff(small_2[, doc_id], small[, doc_id])

small_3 <- tweepy_subset_labeling_lisa[
  favorite_count > 5 & word_count >= 38 & created_at > "2019-01-01"
  & followers_count > 12000 & length(unlist(emojis)) > 0]

new_rows_2 <- setdiff(
  small_3[ , doc_id],
  c(small_2[, doc_id], small[, doc_id]))

fwrite(
  small[
    , .(doc_id, username, retweet_count, favorite_count, full_text)], 
  file = "C:/Users/wimme/Desktop/data-labeling.csv",
  sep = ";")

fwrite(
  small_2[doc_id %in% new_rows][
    , .(doc_id, username, retweet_count, favorite_count, full_text)], 
  file = "C:/Users/wimme/Desktop/data-labeling-2.csv",
  sep = ";")

fwrite(
  small_3[doc_id %in% new_rows_2][
    , .(doc_id, username, retweet_count, favorite_count, full_text)], 
  file = "C:/Users/wimme/Desktop/data-labeling-3.csv",
  sep = ";")


