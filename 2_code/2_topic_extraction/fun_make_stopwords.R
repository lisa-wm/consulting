# ------------------------------------------------------------------------------
# STOPWORD CREATION FOR TOPIC MODELING
# ------------------------------------------------------------------------------

# PURPOSE: create list of stopwords to be used in topic modeling (coarser than 
# for sentiment analysis)

make_stopwords_tm <- function() {
  
  # Collect stopwords from various external sources
  
  sw_1 <- quanteda::stopwords("de")
  
  sw_2 <- XML::xmlToDataFrame(XML::xmlParse(here(
    "2_code/3_sentiment_analysis/1_dict_based/dicts", 
    "german_stopwords.xml"), 
    encoding = "UTF-8")) %>% 
    unlist()
  
  sw_3 <- read.delim(here(
    "2_code/3_sentiment_analysis/1_dict_based/dicts", 
    "stopwords-iso.txt"), encoding = "UTF-8") %>% 
    unlist()
  
  sw <- sort(unique(remove_umlauts(c(sw_1, sw_2, sw_3))))
  
  # Add some domain-specific stopwords
  
  # sw_extended <- c(sw, c(
  #   "politik",
  #   "politisch",
  #   "^bundesregier",
  #   "^bundestag",
  #   "^deutsch",
  #   "berlin",
  #   "prozent"))
  # 
  # stringr::str_remove_all(
  #   stopwords, 
  #   pattern = stringr::str_c(c(
  #     "gegen", 
  #     "^kein",
  #     "^nicht",
  #     "sehr",
  #     "^besser",
  #     "^beste",
  #     "^gut",
  #     "^gern",
  #     "kaum",
  #     "nein",
  #     "nie",
  #     "^niemand",
  #     "^richtig",
  #     "^schlecht",
  #     "^(polit",
  #     "^bundesregier",
  #     "^bundestag",
  #     "^deutsch",
  #     "berlin",
  #     "prozent)"),
  #     collapse = "|"))
  
}
