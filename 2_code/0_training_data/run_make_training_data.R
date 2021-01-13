# ------------------------------------------------------------------------------
# TRAINING DATA (MANUALLY ANNOTATED)
# ------------------------------------------------------------------------------

# Purpose: append manual annotations to tweets subset

load(here(
  "2_code/attic", "tweepy_df_subset_processed.RData"
))

tweets_annotated <- fread(
  here("2_code/0_training_data", "data-tweepy-df-subset-labeled-manually.csv"),
  encoding = "UTF-8",
  sep = ";")

training_data_annotated <- tweepy_df_subset_processed[
  tweets_annotated[, .(doc_id, label, topic)], on = "doc_id"
]

save_rdata_files(training_data_annotated, "2_code/0_training_data")

# ROUND 2 ----------------------------------------------------------------------

load_rdata_files(training_data_annotated, "2_code/0_training_data")

training_data_annotated[
  , rank_timestamp := seq_len(.N),
  by = .(username, created_at)
  ][, doc_id_new := paste(
    username,
    as.character(as.numeric(as.POSIXct(created_at))),
    rank_timestamp,
    sep = ""),
    by = seq_len(nrow(training_data_annotated))]

save_rdata_files(training_data_annotated, "2_code/0_training_data")