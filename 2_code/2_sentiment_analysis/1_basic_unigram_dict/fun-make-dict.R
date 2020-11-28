# ------------------------------------------------------------------------------
# CREATION OF DICTIONARIES
# ------------------------------------------------------------------------------

# Purpose: create n-gram dictionary

# HELPER FUNCTIONS -------------------------------------------------------------

make_dict_positive <- function() {
  
  dict_1 <- foo
    
  # Collect all and remove duplicates as well as umlauts
  
  find_this <- apropos("^sw_[1-99]")
  look_here <- sys.frame(sys.parent(0))
  
  stop_words <- unique(
    remove_umlauts(
      unlist(mget(find_this, envir = look_here)))
  )
  
}

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

data(sentiments, package = "tidytext")
my_dict <- as.dictionary(sentiments)