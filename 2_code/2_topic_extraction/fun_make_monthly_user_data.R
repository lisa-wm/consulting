# ------------------------------------------------------------------------------
# CREATION OF PER-MONTH PER-USER DATA
# ------------------------------------------------------------------------------

# Purpose: aggregate raw tweets per user and month

# HELPER FUNCTIONS -------------------------------------------------------------

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

make_monthly_user_data <- function(data) {
  
  data_mu <- copy(data)[
    , .(name_matching, username, created_at, full_text)
  ][, `:=` (year = year(created_at), month = month(created_at))]
  
  tweets_concat <- data_mu[
    , paste(full_text, collapse = " "), 
    by = .(username, year, month)]
  setnames(tweets_concat, "V1", "full_text_user_month")
  
  unique(data_mu[
    , .(name_matching, username, year, month)
  ])[tweets_concat, on = list(username, year, month)
  ][, doc_id := .I]
  
}
