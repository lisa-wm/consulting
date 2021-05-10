# RECOVER DATA FIRST LABELING ROUND --------------------------------------------

# Asmik

labeling_asmik <- fread(
  here("2_code/1_data/1_training_data", "labeling_asmik_final.csv"),
  encoding = "UTF-8",
  sep = ";")

labeling_asmik <- labeling_asmik[!is.na(retweet_count)]

load(here(
  "2_code/1_data/1_training_data/attic", 
  "rdata_tweepy_subset_labeling_asmik.RData"))

data_round_1_asmik <- tweepy_subset_labeling_asmik

data_round_1_labeled_asmik <- labeling_asmik[
  , .(username, full_text, label)
  ][data_round_1_asmik, on = c("full_text", "username")
    ][!is.na(label)]

data_round_1_labeled_asmik[, created_at := as.character(created_at)]
data.table::setnames(
  data_round_1_labeled_asmik, 
  "full_text", 
  "full_text_processed")

tweets_original_1 <- fread(
  here("1_scraping/output/202010113_2146", "tweepy_df_subset.csv"),
  encoding = "UTF-8",
  sep = ",")

tweets_original_1 <- tweets_original_1[
  , .(username, created_at, full_text)]

data_round_1_original_asmik <- tweets_original_1[
  data_round_1_labeled_asmik, on = c("username", "created_at")
  ][!is.na(full_text)]

# ------------------------------------------------------------------------------

# Lisa

labeling_lisa_1 <- fread(
  here("2_code/1_data/1_training_data/attic", "labeling_lisa_I.csv"),
  encoding = "UTF-8",
  sep = ";")

labeling_lisa_1 <- labeling_lisa_1[nchar(label) > 0L]

load(here(
  "2_code/1_data/1_training_data/attic", 
  "rdata_tweepy_subset_labeling_lisa.RData"))

data_round_1_lisa <- tweepy_subset_labeling_lisa

data_round_1_labeled_lisa <- labeling_lisa_1[
  , .(username, full_text, label)
  ][data_round_1_lisa, on = c("full_text", "username")
    ][!is.na(label)]

data_round_1_labeled_lisa[, created_at := as.character(created_at)]
data.table::setnames(
  data_round_1_labeled_lisa, 
  "full_text", 
  "full_text_processed")

data_round_1_original_lisa <- tweets_original_1[
  data_round_1_labeled_lisa, on = c("username", "created_at")
  ][!is.na(full_text)]

# ------------------------------------------------------------------------------

data.table::setcolorder(
  data_round_1_original_asmik, 
  names(data_round_1_original_lisa))

cols_to_keep <- c(
  "name_matching",
  "username",
  "available",
  "created_at",
  "full_text",
  "retweet_count",
  "favorite_count",
  "followers_count",
  "location",
  "label")

data_round_1_original <- unique(rbind(
  data_round_1_original_asmik[, ..cols_to_keep], 
  data_round_1_original_lisa[, ..cols_to_keep]))

# RECOVER DATA SECOND LABELING ROUND -------------------------------------------

# Asmik

load(here(
  "2_code/1_data/1_training_data/attic", 
  "rdata_tweepy_subset_labeling_asmik_2.RData"))

data_round_2_asmik <- tweepy_subset_labeling_asmik_2

data_round_2_labeled_asmik <- labeling_asmik[
  , .(username, full_text, label)
  ][data_round_2_asmik, on = c("full_text", "username")
    ][!is.na(label)]

data_round_2_labeled_asmik[, created_at := as.character(created_at)]
data.table::setnames(
  data_round_2_labeled_asmik, 
  "full_text", 
  "full_text_processed")

tweets_original_2 <- fread(
  here("1_scraping/output/202010113_2146", "tweepy_df_subset_no_retweets.csv"),
  encoding = "UTF-8",
  sep = ",")

tweets_original_2 <- tweets_original_2[
  , .(username, created_at, full_text)]

data_round_2_original_asmik <- tweets_original_2[
  data_round_2_labeled_asmik, on = c("username", "created_at")
  ][!is.na(full_text)]

# ------------------------------------------------------------------------------

# Lisa

labeling_lisa_2 <- fread(
  here("2_code/1_data/1_training_data/attic", "labeling_lisa_II.csv"),
  encoding = "UTF-8",
  sep = ";")

labeling_lisa_2 <- labeling_lisa_2[nchar(label) > 0L]

load_rdata_files(data_clean, folder = "2_code/1_data/2_tmp_data")

cols_to_keep_2 <- c(
  "doc_id",
  "meta_name_matching",
  "twitter_username",
  "twitter_available",
  "twitter_created_at",
  "twitter_retweet_count",
  "twitter_favorite_count",
  "twitter_followers_count",
  "twitter_location")

data_round_2_labeled_lisa <- data_clean[
  , ..cols_to_keep_2
  ][labeling_lisa_2, on = "doc_id"]

data.table::setnames(
  data_round_2_labeled_lisa,
  c("doc_id",
    "name_matching",
    "username",
    "available",
    "created_at",
    "retweet_count",
    "favorite_count",
    "followers_count",
    "location",
    "full_text_processed",
    "topic",
    "label"))

data_round_2_labeled_lisa[, created_at := as.character(created_at)]

data_round_2_original_lisa <- tweets_original_2[
  data_round_2_labeled_lisa, on = c("username", "created_at")
  ][!is.na(full_text)]

# ------------------------------------------------------------------------------

data_round_2_original_asmik <- data_round_2_original_asmik[, ..cols_to_keep]
data_round_2_original_lisa <- data_round_2_original_lisa[, ..cols_to_keep]

data.table::setcolorder(
  data_round_2_original_asmik, 
  names(data_round_1_original))

data.table::setcolorder(
  data_round_2_original_lisa, 
  names(data_round_1_original))

data_round_2_original <- unique(rbind(
  data_round_2_original_asmik, 
  data_round_2_original_lisa))

# COLLECT & SAVE ---------------------------------------------------------------

data_labeled <- unique(rbind(data_round_1_original, data_round_2_original))

save_rdata_files(
  data_labeled,
  folder = "2_code/1_data/1_training_data", 
  tmp = FALSE)

data.table::fwrite(
  data_labeled,
  here("2_code/1_data/1_training_data", "data_labeled.csv"),
  sep = ";")
