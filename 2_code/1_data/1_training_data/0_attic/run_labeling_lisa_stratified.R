load_rdata_files(data_clean, folder = "2_code/1_data/2_tmp_data")

labeling_lisa_second_round <- data_clean[
  label == "none" & twitter_word_count > 25L &
    (lengths(twitter_emojis) > 0L | lengths(twitter_hashtags) > 0L)]

data.table::fwrite(
  labeling_lisa_second_round[, .(doc_id, full_text = twitter_full_text)], 
  file = here("2_code/1_data/1_training_data", "labeling_lisa_II_todo.csv"),
  sep = ";")
