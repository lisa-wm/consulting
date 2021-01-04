# ------------------------------------------------------------------------------
# CREATION OF PER-MONTH PER-USER DATA
# ------------------------------------------------------------------------------

# Purpose: aggregate raw tweets per user and month

append_covariates <- function(tweets_data, mp_data, se_data) {
  
  # Merge all three, removing duplicate columns on the go
  # Format looks strange but is data.table's way of performing a left join
  
  tweets_data_mp <- mp_data[tweets_data, on = "name_matching"]
  tweets_data_mp_se <- se_data[
    tweets_data_mp, 
    mget(union(names(tweets_data_mp), names(se_data))),
    on = "wahlkreis_nr"]

  # FIXME Find out whether asterisks are really resigned MPs
  
  tweets_data_mp_se <- tweets_data_mp_se[
    !stringr::str_detect(party, "\\*")
    ][, `:=` (bundesland = as.factor(bundesland), party = as.factor(party))]
  
  # FIXME Drop MPs w/o wahlkreis_nr (happens through inner join in S & P model)
  # Substitute by bundesland-wide average or similar workaround
  
  tweets_data_mp_se <- tweets_data_mp_se[!is.na(wahlkreis_nr)]
  
  # Drop MP w/o political party
  
  tweets_data_mp_se <- tweets_data_mp_se[party != "fraktionslos"]
  
  # Change party levels to single, lowercase character
  
  levels(tweets_data_mp_se$party) <- c(
    "afd", "gruene", "cdu_csu", "linke", "fdp", "fraktionslos", "spd"
  )
  
  # Get electoral result for MP's party in separate variable
  # Throws warning but unjustified, is open issue in data.table
  
  # TODO Make this faster
  
  suppressWarnings(tweets_data_mp_se[
    , vote_share_own_party := sapply(
      .I, 
      function(i) {
        this_party <- as.character(tweets_data_mp_se[i, party])
        as.numeric(tweets_data_mp_se[i, ..this_party])
        })])
  
  # Create time index to be included as smooth effect
  
  tweets_data_mp_se[
    , `:=` (year = year(created_at), month = month(created_at))
    ][, time_index := frank(list(year, month), ties.method = "min")]
 
}
