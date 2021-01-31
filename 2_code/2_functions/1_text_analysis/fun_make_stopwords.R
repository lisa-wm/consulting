# ------------------------------------------------------------------------------
# STOPWORD CREATION
# ------------------------------------------------------------------------------

# PURPOSE: create basic list of stopwords to be used in topic modeling (coarser than 
# for sentiment analysis)

make_stopwords <- function() {
  
  # Collect stopwords from various external sources
  
  sw_1 <- quanteda::stopwords("de")
  
  sw_2 <- XML::xmlToDataFrame(XML::xmlParse(here(
    "2_code/1_data/0_external_data", 
    "german_stopwords.xml"), 
    encoding = "UTF-8"))
  
  sw_3 <- read.delim(here(
    "2_code/1_data/0_external_data", 
    "stopwords-iso.txt"), encoding = "UTF-8")
  
  sw <- c(sw_1, unlist(sw_2), unlist(sw_3))
  
  sort(
    unique(
      SnowballC::wordStem(
        remove_umlauts(sw), 
        language = "de")))
  
}
