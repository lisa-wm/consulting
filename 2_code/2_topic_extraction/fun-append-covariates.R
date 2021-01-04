# ------------------------------------------------------------------------------
# CREATION OF PER-MONTH PER-USER DATA
# ------------------------------------------------------------------------------

# Purpose: aggregate raw tweets per user and month

append_covariates <- function(tweets_data, mp_data, se_data) {
  
  # FIXME Contains multiple (non-duplicate) rows per wahlkreis_nr
  
  se_data_unique <- se_data[
    , head(.SD, 1), by = wahlkreis_nr]
  
  # Merge all three
  # Format looks strange but is data.table's way of performing a left join
  
  tweets_data_mp <- mp_data[tweets_data, on = "name_matching"]
  tweets_data_mp_se <- se_data_unique[tweets_data_mp, on = "wahlkreis_nr"]
  
  # FIXME Change the following in Jupyter
  
  tweets_data_mp_se[, i.bundesland := NULL]
  
  # FIXME Find out whether asterisks are really resigned MPs
  
  tweets_data_mp_se <- tweets_data_mp_se[
    !stringr::str_detect(party, "\\*")
  ][, `:=` (bundesland = as.factor(bundesland), party = as.factor(party))]
  
  # Change party levels to single, lowercase character
  
  levels(tweets_data_mp_se$party) <- c(
    "afd", "gruene", "cdu_csu", "linke", "fdp", "fraktionslos", "spd"
  )
  
  # Get electoral result for MP's party in separate variable
  
  electoral_result <- as.data.table(na.omit(unique(se_data_unique[
    , .(wahlkreis_nr, spd, linke, gruene, fdp, afd, cdu_csu)])) %>% 
    gather("party", "vote_share_own_party", -wahlkreis_nr))

  tweets_data_mp_se_er <- merge(
    tweets_data_mp_se, 
    electoral_result, 
    by = c("wahlkreis_nr", "party"),
    all = TRUE)
  
  # Create time index to be included as smooth effect
  
  tweets_data_mp_se_er[
    , `:=` (year = year(created_at), month = month(created_at))
    ][, time_index := frank(list(year, month), ties.method = "min")]
  
  # Strange by-product of join
  
  tweets_data_mp_se_er <- tweets_data_mp_se_er[!is.na(doc_id)]
  
  # FIXME Drop MPs w/o wahlkreis_nr (happens through inner join in S & P model)
  # Substitute by bundesland-wide average or similar workaround
  
  tweets_data_mp_se_er <- tweets_data_mp_se_er[!is.na(wahlkreis_nr)]

  # Drop MP w/o political party
  
  tweets_data_mp_se_er <- tweets_data_mp_se_er[party != "fraktionslos"]
  
}
