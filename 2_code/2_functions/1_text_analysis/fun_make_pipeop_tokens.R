# ------------------------------------------------------------------------------
# TOKENIZATION PIPEOP
# ------------------------------------------------------------------------------

# PURPOSE: create pipe operator for tokenization

# MAKE PIPEOP ------------------------------------------------------------------

PipeOpStratifyKeywords = R6::R6Class(
  
  "PipeOpStratifyKeywords",
  
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  
  public = list(
    
    initialize = function(id = "stratify_keywords", param_vals = list()) {
      
      ps = ParamSet$new(params = list(
        ParamUty$new("docid_field"),
        ParamUty$new("text_field"),
        ParamUty$new("stopwords"),
        ParamUty$new("keywords")
      ))
      
      # ps$values <- list()
      
      super$initialize(
        id = id, 
        param_set = ps, 
        param_vals = param_vals,
        packages = c("quanteda"))
      
    }
  ),
  
  private = list(
    
    .transform_dt = function(dt, levels) {
      
      # Transform data

      crp <- quanteda::corpus(
        dt,
        docid_field = self$param_set$values$docid_field,
        text_field = self$param_set$values$text_field)

      tkns <- private$.tokenize(
        corpus = crp, 
        stopwords = self$param_set$values$stopwords)
      
      dfm <- quanteda::dfm(tkns)
      
      # Match with keywords
      
      keywords <- lapply(
        self$param_set$values$keywords,
        function(i) SnowballC::wordStem(remove_umlauts(i), language = "de"))
      
      mtch <- convert_qtda_to_dt(
        quanteda::dfm_match(dfm, unlist(keywords)),
        key = "doc_id")
      
      mtch_cols <- names(mtch)[names(mtch) != "doc_id"]
      
      mtch <- mtch[
        , is_match_any := sum(.SD) > 0L,
        .SDcols = mtch_cols,
        by = seq_len(nrow(mtch))
        ][, .(doc_id, is_match_any)]
      
      dt_new <- data.table::copy(dt)
      
      dt_new <- mtch[dt_new, on = "doc_id"]
      
      dt_new

    },

    .tokenize = function(corpus, stopwords) {

      tkns <- quanteda::tokens(
        corpus,
        what = "word",
        remove_symbols = TRUE,
        remove_numbers = TRUE,
        remove_separators = TRUE,
        split_hyphens = TRUE,
        include_docvars = TRUE)

      tkns <- quanteda::tokens_wordstem(tkns, language = "german")

      tkns_topics <- quanteda::tokens_keep(
        tkns,
        pattern = c("[[:upper:]]([[:lower:]])+"),
        valuetype = "regex",
        case_insensitive = FALSE)

      tkns_topics <- quanteda::tokens_remove(
        quanteda::tokens_tolower(tkns_topics),
        pattern = stopwords)

      tkns_topics

    }
  )
)
