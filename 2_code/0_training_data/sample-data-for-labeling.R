# ------------------------------------------------------------------------------
# SAMPLE PREPROCESSED DATA FOR MANUAL LABELING
# ------------------------------------------------------------------------------

library(here)

load(here("2_code", "tweepy_df_subset_processed.RData"))

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

save(tweepy_subset_labeling_asmik, 
     file = here("2_code", "tweepy_subset_labeling_asmik.RData"))
save(tweepy_subset_labeling_lisa, 
     file = here("2_code", "tweepy_subset_labeling_lisa.RData"))