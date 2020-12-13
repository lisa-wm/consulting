# ------------------------------------------------------------------------------
# SENTIMENT CLASSIFICATION BASED ON N-GRAM DICTIONARY
# ------------------------------------------------------------------------------

# Purpose: document-level SA, positive/negative, using an n-gram dictionary

# HELPER FUNCTIONS -------------------------------------------------------------

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

make_sentiments_dict <- function(dfm, dictionary) {
  
  sentiments_df <- convert(
    dfm_lookup(dfm, dictionary),
    to = "data.frame") %>% 
    as.data.table()
  
  sentiments_df[, `:=` (
    doc_id = as.numeric(doc_id),
    sentiments_found = positive + negative,
    diff_pos = positive - negative,
    label = case_when(
      positive - negative > 0 ~ "positive",
      positive - negative < 0 ~ "negative",
      positive - negative == 0 ~ "indecisive"))]
  
  median_sentiments_found <- median(sentiments_df[, sentiments_found])
  
  sentiments_df[, confidence_factor := 
                  sentiments_found / ..median_sentiments_found]
  
}

