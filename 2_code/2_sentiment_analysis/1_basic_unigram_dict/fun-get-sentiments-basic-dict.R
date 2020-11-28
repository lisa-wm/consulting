# ------------------------------------------------------------------------------
# SENTIMENT CLASSIFICATION BASED ON N-GRAM DICTIONARY
# ------------------------------------------------------------------------------

# Purpose: document-level SA, positive/negative, using an n-gram dictionary

# HELPER FUNCTIONS -------------------------------------------------------------

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

get_sentiments_basic_unigram_dict <- function(dfm, dictionary) {
  
  sentiments_df <- convert(
    dfm_lookup(dfm, dictionary),
    to = "data.frame") %>%
    mutate(
      sentiments_found = positive + negative,
      diff_pos = positive - negative,
      label = case_when(
        diff_pos > 0 ~ "positive",
        diff_pos < 0 ~ "negative",
        diff_pos == 0 ~ "indecisive")
      ) %>% 
    as.data.table()
  
}

