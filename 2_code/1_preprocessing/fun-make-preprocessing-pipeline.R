# ------------------------------------------------------------------------------
# PREPROCESSING PIPELINE
# ------------------------------------------------------------------------------

# Purpose: make mlr3 pipeline for preprocessing data

# HELPER FUNCTIONS -------------------------------------------------------------

# Create mlr3 pipe operator for textual preprocessing

make_preprocessing_pipeop <- function(ngram) {
  
  mlr3pipelines::po(
    "textvectorizer",
    id = "textpreprocessing",
    param_vals = list(
      extra_stopwords = make_stopwords(),
      tolower = TRUE,
      stem = TRUE,
      what = "word",
      n = ngram,
      remove_punct = TRUE,
      remove_numbers = TRUE,
      min_termfreq = 1,
      scheme_df = "inverse"))
  
}

# TOP-LEVEL FUNCTIONS ----------------------------------------------------------

# Create mlr3 graph, where one branch takes care of preprocessing the text
# column while the other just passes the remaining features on

make_preprocessing_pipeline <- function(text_column, ngram = 1L) {
  
  # Perform basic input checks
  
  checkmate::assert_string(text_column)
  checkmate::assert_count(ngram)
  
  # Branch off text column for preprocessing
 
  to_process <- mlr3pipelines::selector_grep(text_column)
  rest <- mlr3pipelines::selector_invert(to_process)
   
  # Create pipeline
  
  po_preprocessing <- make_preprocessing_pipeop(ngram = ngram)
  
  mlr3pipelines::gunion(list(
    mlr3pipelines::po("select", selector = to_process, id = "tweets") %>>% 
      po_preprocessing,
    mlr3pipelines::po("select", selector = rest, id = "rest"))) %>>%
    mlr3pipelines::po("featureunion")
  
}
