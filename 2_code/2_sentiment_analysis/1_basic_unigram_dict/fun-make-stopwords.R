# ------------------------------------------------------------------------------
# COLLECTION OF STOPWORDS
# ------------------------------------------------------------------------------

# Purpose: create tokens out of basic corpus

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

# TODO Make good stopword list

get_stopwords <- function() {
  
  # Collect stopwords from various sources
  
  sw_1 <- stopwords("de")
  
  sw_2 <- xmlToDataFrame(xmlParse(here(
    "2_code/2_sentiment_analysis/1_basic_unigram_dict/dicts", 
    "german_stopwords.xml"), 
    encoding = "UTF-8")) %>% 
    unlist()
  
  sw_3 <- read.delim(here(
    "2_code/2_sentiment_analysis/1_basic_unigram_dict/dicts", 
    "stopwords-iso.txt"), encoding = "UTF-8") %>% 
    unlist()
  
  # Collect all and remove duplicates as well as umlauts
  
  # FIXME Does not work for some reason
  
  # find_this <- apropos("^sw_[1-99]")
  # look_here <- sys.frame(sys.parent(0))
  # 
  # stopwords <- unique(
  #   remove_umlauts(
  #     unlist(mget(find_this, envir = look_here)))
  # )
  # Try: look_here <- sys.frame(sys.nframe())
  
  stopwords <- sort(unique(remove_umlauts(c(sw_1, sw_2, sw_3))))
  
  # Remove words deemed important for sentiment
  
  stringr::str_remove_all(
    stopwords, 
    pattern = stringr::str_c(c(
      "gegen", 
      "^kein",
      "^nicht",
      "sehr",
      "^besser",
      "^beste",
      "^gut",
      "^gern",
      "kaum",
      "nein",
      "nie",
      "^niemand",
      "^richtig",
      "^schlecht"),
      collapse = "|"))
  
}
