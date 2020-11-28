# ------------------------------------------------------------------------------
# CREATION OF DICTIONARIES
# ------------------------------------------------------------------------------

# Purpose: create n-gram dictionary

# HELPER FUNCTIONS -------------------------------------------------------------

make_dict_germanpolarityclues <- function(file) {
  
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
  
  # TODO Make this better, horribly inefficient
  
  dict_compact <- data.frame(
    lemma = character(),
    pos_tag = character(),
    polarity = character(),
    score_pos_neg_neut = character(),
    derivatives = character()
  )
  
  for (i in unique(dict$lemma)) {
    
    this_df <- dict %>% filter(lemma == i)
    start_col <- ncol(dict_compact)
    
    this_df <- unique(this_df) %>%
      spread(word, 1) 
    
    # FIXME If same lemma occurs w/ different scores (might happen for variants
    # in pronunciation, such as "Abschluss" vs "Abschluß"), brute force way is
    # currently to just pick first row
    
    if (nrow(this_df) > 1) {this_df <- this_df[1, ]}
    
    end_col <- ncol(this_df)
    
    this_df <- this_df %>% 
      unite(all_of(start_col):all_of(end_col), col = "derivatives", sep = ",")
    dict_compact <- bind_rows(dict_compact, this_df)
    
  }

  dict_compact %>% 
    mutate(derivatives = stringr::str_split(derivatives, ","))
  
}

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

# Creates only very basic dictionary, omitting scores, POS tags etc.

create_unigram_dict <- function(source_positive, source_negative) {
  
  dict_pos <- make_dict_germanpolarityclues(source_positive)
  dict_neg <- make_dict_germanpolarityclues(source_negative)
 
  quanteda::dictionary(list(
    positive = unlist(dict_pos$derivatives),
    negative = unlist(dict_neg$derivatives)))
  
}

