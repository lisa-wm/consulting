# ------------------------------------------------------------------------------
# CREATION OF SENTIMENT DICTIONARY BY RAUH
# ------------------------------------------------------------------------------

# PURPOSE: read dictionary proposed by Rauh, tidy up

get_dict_rauh <- function() {
  
  load(here::here(
    "2_code/1_data/0_external_data", 
    "Rauh_SentDictionaryGerman.Rdata"))
  
  dict <- data.table::setDT(sent.dictionary)
  data.table::setnames(dict, c("term", "polarity"))
  
  dict[, polarity := as.factor(as.numeric(polarity))]
  levels(dict$polarity) <- c("negative", "positive")
  
  dict[
    , term := remove_umlauts(tolower(term))
    ][, term := SnowballC::wordStem(term, language = "de")]
  
  dict
  
}
