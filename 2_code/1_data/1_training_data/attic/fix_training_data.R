# Recover data Asmik labeled

labeling_asmik <- fread(
  here("2_code/1_data/1_training_data", "labeling_asmik_final.csv"),
  encoding = "UTF-8",
  sep = ";")

labeling_asmik <- labeling_asmik[!is.na(username)]

cols_to_keep <- c(
  "name_matching",
  "username",
  "available",
  "created_at",
  "full_text",
  "retweet_count",
  "favorite_count",
  "followers_count",
  "location")

# First, load old tweepy_subset* from attic

data_old_asmik_1 <- tweepy_subset_labeling_asmik[, ..cols_to_keep]
data_old_asmik_2 <- tweepy_subset_labeling_asmik_2[
  , ..cols_to_keep
  ][, created_at := as.POSIXct(created_at)]

data_old_asmik_1_annotated <- labeling_asmik[
  , .(username, full_text, label)
  ][data_old_asmik_2, on = c("full_text", "username")
    ][!is.na(label)]

data_old_asmik_2_annotated <- labeling_asmik[
  , .(username, full_text, label)
  ][data_old_2, on = c("full_text", "username")
    ][!is.na(label)]

labeling_asmik_final <- unique(
  rbind(data_old_asmik_1_annotated, data_old_asmik_2_annotated))

save_rdata_files(
  labeling_asmik_final, 
  folder = "2_code/1_data/1_training_data",
  tmp = FALSE)

# ------------------------------------------------------------------------------

# Recover data Lisa labeled

labeling_lisa_1 <- fread(
  here("2_code/1_data/1_training_data", "labeling_lisa_I.csv"),
  encoding = "UTF-8",
  sep = ";")

data_old_lisa <- tweepy_subset_labeling_lisa[, ..cols_to_keep]

data_old_lisa_annotated <- labeling_lisa_1[
  , .(username, full_text, label, topic)
  ][data_old_lisa, on = c("full_text", "username")
    ][label %in% c("positive", "negative")]

load_rdata_files(data_clean, folder = "2_code/1_data/2_tmp_data")

labeling_lisa_2 <- fread(
  here("2_code/1_data/1_training_data/attic", "labeling_lisa_II.csv"),
  encoding = "UTF-8",
  sep = ";")

labeling_lisa_2 <- labeling_lisa_2[label %in% c("positive", "negative")]

labeling_lisa_2 <- data_clean[
  , .(doc_id, twitter_username, twitter_created_at)
  ][labeling_lisa_2, on = "doc_id"
    ][, .(username = twitter_username, 
          created_at = twitter_created_at, 
          topic, 
          label)]

# cols_to_keep_new <- c(
#   "doc_id",
#   "meta_name_matching",
#   sprintf("twitter_%s", cols_to_keep[2L:length(cols_to_keep)]))

load_rdata_files(tweets_raw_new, folder = "2_code/1_data/2_tmp_data")
data_new_lisa <- tweets_raw_new[
  , ..cols_to_keep
  ][, full_text := remove_noisy_symbols(remove_umlauts(full_text))]

# data_new_lisa <- data_clean[, ..cols_to_keep_new]

data_new_lisa_annotated <- data_new_lisa[
  labeling_lisa_2[, .(username, created_at, topic, label)],
  on = c("username", "created_at")]

# data.table::setnames(
#   data_new_lisa_annotated, 
#   c("doc_id", cols_to_keep, "topic", "label"))
# 
# data_new_lisa_annotated <- data_new_lisa_annotated[
#   , .(username, full_text, label, topic, name_matching, available, created_at,
#       retweet_count, favorite_count, followers_count, location)]

data.table::setcolorder(data_old_lisa_annotated, names(data_new_lisa_annotated))

labeling_lisa <- unique(
  rbind(data_old_lisa_annotated, data_new_lisa_annotated))

labeling_lisa[
  , `:=` (
    from = stringr::str_locate(full_text, topic)[, 1L],
    to = stringr::str_locate(full_text, topic)[, 2L])]

errors <- labeling_lisa[is.na(from) | is.na(to)]

labeling_lisa_final <- labeling_lisa[!(is.na(from) | is.na(to))]

save_rdata_files(
  labeling_lisa_final, 
  folder = "2_code/1_data/1_training_data",
  tmp = FALSE)

data.table::fwrite(
  labeling_lisa_final,
  file = here("2_code/1_data/1_training_data", "labeling_lisa_final.csv"),
  sep = ";")
