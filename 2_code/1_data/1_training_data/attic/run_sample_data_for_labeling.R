# ------------------------------------------------------------------------------
# SAMPLE PREPROCESSED DATA FOR MANUAL LABELING
# ------------------------------------------------------------------------------

library(here)

# FIRST ROUND ------------------------------------------------------------------

load(here("2_code/attic", "tweepy_df_subset_processed.RData"))

set.seed(123)

labels_asmik <- sample(
  seq_len(nrow(tweepy_df_subset_processed)), 
  0.5 * nrow(tweepy_df_subset_processed),
  replace = FALSE)

tweepy_subset_labeling_asmik <- tweepy_df_subset_processed[labels_asmik]
tweepy_subset_labeling_lisa <- tweepy_df_subset_processed[!labels_asmik]

(length(intersect(
  tweepy_subset_labeling_asmik$doc_id, 
  tweepy_subset_labeling_lisa$doc_id)))

save(
  tweepy_subset_labeling_asmik, 
    file = here(
      "2_code/0_training_data", 
      "rdata_tweepy_subset_labeling_asmik.RData"))
save(
  tweepy_subset_labeling_lisa, 
  file = here(
    "2_code/0_training_data", 
    "rdata_tweepy_subset_labeling_lisa.RData"))

# SECOND ROUND -----------------------------------------------------------------

load(here("2_code/attic", "rdata_data_clean.RData"))

set.seed(123)

labels_asmik_2 <- sample(
  seq_len(nrow(data_clean)), 
  0.5 * nrow(data_clean),
  replace = FALSE)

tweepy_subset_labeling_asmik_2 <- data_clean[labels_asmik_2]
tweepy_subset_labeling_lisa_2 <- data_clean[!labels_asmik_2]

# Check for overlaps

stopifnot(nrow(tweepy_subset_labeling_asmik_2) - 
  length(unique(tweepy_subset_labeling_asmik_2$doc_id_new)) == 0)

stopifnot(nrow(tweepy_subset_labeling_lisa_2) - 
  length(unique(tweepy_subset_labeling_lisa_2$doc_id_new)) == 0)

stopifnot(length(intersect(
  tweepy_subset_labeling_asmik_2$doc_id, 
  tweepy_subset_labeling_lisa_2$doc_id)) == 0)

load_rdata_files(training_data_annotated, "2_code/0_training_data")

stopifnot(length(intersect(
  tweepy_subset_labeling_asmik_2$doc_id_new,
  training_data_annotated$doc_id_new)) == 0)

stopifnot(length(intersect(
  tweepy_subset_labeling_lisa_2$doc_id_new,
  training_data_annotated$doc_id_new)) == 0)

save_rdata_files(tweepy_subset_labeling_asmik_2, "2_code/0_training_data")
save_rdata_files(tweepy_subset_labeling_lisa_2, "2_code/0_training_data")

load_rdata_files(
  tweepy_subset_labeling_asmik_2, 
  "2_code/1_data/1_training_data")