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

# first, load old tweepy_subset* from attic

data_old_1 <- tweepy_subset_labeling_asmik[, ..cols_to_keep]
data_old_2 <- tweepy_subset_labeling_asmik_2[
  , ..cols_to_keep
  ][, created_at := as.POSIXct(created_at)]

data_old_1_annotated <- labeling_asmik[
  , .(username, full_text, label)
  ][data_old_1, on = c("full_text", "username")
    ][!is.na(label)]

data_old_2_annotated <- labeling_asmik[
  , .(username, full_text, label)
  ][data_old_2, on = c("full_text", "username")
    ][!is.na(label)]

labeling_asmik_final <- unique(
  rbind(data_old_1_annotated, data_old_2_annotated))

save_rdata_files(
  labeling_asmik_final, 
  folder = "2_code/1_data/1_training_data",
  tmp = FALSE)

