# ------------------------------------------------------------------------------
# STOPWORD CREATION FOR SENTIMENT ANALYSIS
# ------------------------------------------------------------------------------

# PURPOSE: create list of stopwords to be used in sentiment analysis (more 
# fine-grained than for topic modeling)

make_stopwords_sa <- function() {
  
  # Collect stopwords from various external sources
  
  sw_1 <- quanteda::stopwords("de")
  
  sw_2 <- XML::xmlToDataFrame(XML::xmlParse(here(
    "2_code/0_external_data", 
    "german_stopwords.xml"), 
    encoding = "UTF-8")) %>% 
    unlist()
  
  sw_3 <- read.delim(here(
    "2_code/0_external_data", 
    "stopwords-iso.txt"), encoding = "UTF-8") %>% 
    unlist()
  
  # Additional removal of some domain-specific stopwords (after stemming so no
  # conjugations must be provided)
  
  sw <- c(c(sw_1, sw_2, sw_3), c(
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
  
  # Remove some stopwords from list that might help in classifying sentiment
  
  sw <- stringr::str_remove_all(
    sw,
    pattern = stringr::str_c(c(
      "",
      "gegen",
      "kein",
      "nicht",
      "sehr",
      "besser",
      "beste",
      "gut",
      "gern",
      "kaum",
      "nein",
      "nie",
      "niemand",
      "richtig",
      "schlecht"),
      collapse = "|"))
  
  sw %>% 
    tolower() %>% 
    remove_umlauts() %>% 
    SnowballC::wordStem(language = "de") %>% 
    unique() %>% 
    sort()
  
}