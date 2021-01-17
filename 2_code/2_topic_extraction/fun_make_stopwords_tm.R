# ------------------------------------------------------------------------------
# STOPWORD CREATION FOR TOPIC MODELING
# ------------------------------------------------------------------------------

# PURPOSE: create list of stopwords to be used in topic modeling (coarser than 
# for sentiment analysis)

make_stopwords_tm <- function() {
  
  # Collect stopwords from various external sources
  
  sw_1 <- quanteda::stopwords("de")
  
  sw_2 <- XML::xmlToDataFrame(XML::xmlParse(here(
    "2_code/0_external_data/dicts", 
    "german_stopwords.xml"), 
    encoding = "UTF-8")) %>% 
    unlist()
  
  sw_3 <- read.delim(here(
    "2_code/0_external_data/dicts", 
    "stopwords-iso.txt"), encoding = "UTF-8") %>% 
    unlist()
  
  sw <- c(sw_1, sw_2, sw_3) %>% 
    remove_umlauts() %>% 
    SnowballC::wordStem(language = "de") %>% 
    unique() %>% 
    sort()
  
  # Additional removal of some domain-specific stopwords (after stemming so no
  # conjugations must be provided)
  
  c(sw, c(
    "polit",
    "bundesregier",
    "bundestag",
    "deutsch",
    "deutschland",
    "berlin",
    "prozent",
    "herzlich",
    "glueckwunsch",
    "frag",
    "woch",
    "partei"))
  
  
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
