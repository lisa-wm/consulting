# ------------------------------------------------------------------------------
# STRATIFICATION PIPEOP
# ------------------------------------------------------------------------------

# Purpose: create pipe operator for stratification so data can be resampled
# along strata containing keywords

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
        packages = c("quanteda", "SnowballC", "stringi", "stringr"))
      
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
        function(i) {
          SnowballC::wordStem(private$.remove_umlauts(i), language = "de")})
      
      dict <- quanteda::dictionary(keywords)
      
      mtch <- data.table::setDT(
        quanteda::convert(quanteda::dfm_lookup(dfm, dict), to = "data.frame"),
        key = "doc_id")

      mtch_cols <- names(keywords)
      
      mtch <- mtch[
        , sprintf("stratum_%d", seq_along(mtch_cols)) := lapply(
          .SD, 
          function(i) ifelse(i > 0L, 1L, 0L)),
        .SDcols = mtch_cols,
        by = seq_len(nrow(mtch))]
      
      cols_to_keep <- c("doc_id", sprintf("stratum_%d", seq_along(mtch_cols)))
      mtch <- mtch[, ..cols_to_keep]
      
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

    },
    
    .remove_umlauts = function(text) {
      
      checkmate::assert_character(text)
      
      # This is necessary for R to convert all representations of umlauts 
      # (of which there are several) to a single one that can then be reliably 
      # detected 
      
      text <- stringi::stri_trans_general(text, "Any-Latin")
      
      stringr::str_replace_all(
        text, 
        c(
          "\u00c4" = "Ae",
          "\u00e4" = "ae",
          "\u00d6" = "Oe",
          "\u00f6" = "oe",
          "\u00dc" = "Ue",
          "\u00fc" = "ue",
          "\u00df" = "ss"))
      
    }
  )
)
