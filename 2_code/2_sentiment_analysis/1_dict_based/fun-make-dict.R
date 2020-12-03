# ------------------------------------------------------------------------------
# CREATION OF DICTIONARIES
# ------------------------------------------------------------------------------

# Purpose: create n-gram dictionary

# HELPER FUNCTIONS -------------------------------------------------------------

get_germanpolarityclues <- function(file) {
  
  dict <- data.table::fread(
    file,
    encoding = "UTF-8",
    drop = 6,
    col.names = c(
      "word", "lemma", "pos_tag", "polarity", "score_pos_neg_neut")) %>% 
    mutate(
      lemma = remove_umlauts(tolower(lemma)),
      word = remove_umlauts(tolower(word))
    )
  
  # FIXME In case of different scores per lemma (might happen due to umlauts
  # removal), brute force approach here is to take just the first row

  dt <- copy(dict)
  
  dt[, derivatives := lapply(lemma, function(l) dict[lemma == l, word])
     ][, head(.SD, 1), lemma
       ][, -c("word")]

}

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

# Creates only very basic dictionary, omitting scores, POS tags etc.

# TODO Make this faster

make_dict_unigram <- function(source_positive, source_negative) {
  
  dict_pos <- get_germanpolarityclues(source_positive)
  dict_neg <- get_germanpolarityclues(source_negative)
 
  quanteda::dictionary(list(
    positive = unlist(dict_pos$derivatives),
    negative = unlist(dict_neg$derivatives)))
  
}

