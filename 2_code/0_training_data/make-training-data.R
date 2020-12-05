# ------------------------------------------------------------------------------
# TRAINING DATA (MANUALLY ANNOTATED)
# ------------------------------------------------------------------------------

# Purpose: append manual annotations to tweets subset

load(here(
  "2_code", "tweepy_df_subset_processed.RData"
))

tweets_annotated <- fread(
  here("2_code/0_training_data", "tweepy_df_subset_labeled_manually.csv"),
  encoding = "UTF-8",
  sep = ";")

training_data_annotated <- tweepy_df_subset_processed[
  tweets_annotated[, .(doc_id, label, topic)], on = "doc_id"
]

save(
  training_data_annotated,
  file = here("2_code/0_training_data", "training_data_annotated.RData"))
