# ------------------------------------------------------------------------------
# STOPWORD CREATION FOR TOPIC MODELING
# ------------------------------------------------------------------------------

# PURPOSE: create list of stopwords to be used in topic modeling (coarser than 
# for sentiment analysis)

make_stopwords_tm <- function() {
  
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

  
}
