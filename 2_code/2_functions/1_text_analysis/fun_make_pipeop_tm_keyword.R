# ------------------------------------------------------------------------------
# TOPIC MODELING PIPEOP (KEYWORD-BASED)
# ------------------------------------------------------------------------------

# PURPOSE: create mlr3pipelines pipe operator for topic modeling based on 
# keywords

# MAKE PIPEOP ------------------------------------------------------------------

PipeOpExtractTopicsKeyword = R6::R6Class(
  
  "PipeOpExtractTopicsKeyword",
  
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  
  public = list(
    
    initialize = function(id = "extract_topics_keyword", param_vals = list()) {
      
      ps = ParamSet$new(params = list(
        ParamUty$new("docid_field"),
        ParamUty$new("text_field"),
        ParamUty$new("keywords"),
        ParamInt$new("n_byterms")
      ))
      
      # ps$values <- list(
      #   init.type = "Spectral")
      
      super$initialize(
        id = id, 
        param_set = ps, 
        param_vals = param_vals,
        packages = c("quanteda", "stm"))
      
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
      
      fcm <- quanteda::fcm(dfm, tri = FALSE)
      
      # Prepare keywords list
      
      keywords_list <- private$.make_keywords_list(
        keywords = self$param_set$values$keywords,
        fcm = fcm)
      
      dict_keywords <- quanteda::dictionary(keywords_list)
      
      # matches_dfm <- quanteda::dfm_lookup(
      #   tweets_dfm_tm, 
      #   dict_keywords,
      #   levels = 1:3)
      # 
      # tweets_matches <- convert_qtda_to_dt(matches_dfm, key = "doc_id")
      
      dt_new <- copy(dt)[, foo := class(dict_keywords)[1]]
      
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
    
    .find_duplicate_occurrence = function(data) {
      
      data_raw <- c(t(data))
      
      count_cum <- sapply(
        seq_along(data_raw), 
        function(i) sum(data_raw[1:i] == data_raw[i]))
      
      count_cum_dt <- as.data.table(
        matrix(count_cum, ncol = ncol(data), byrow = TRUE))
      setnames(count_cum_dt, names(data))
      
      count_cum_dt > 1
      
    },
    
    .make_keywords_list = function(keywords, fcm) {
      
      keywords <- lapply(
        keywords,
        function(i) SnowballC::wordStem(remove_umlauts(i), language = "de"))
      
      if (any(!names(keywords) %in% quanteda::featnames(fcm))) {
        stop(sprintf(
          "keyword %s not in data, please change selection", 
          names(keywords)[
            which(!names(keywords) %in% quanteda::featnames(fcm))]))
      }
      
      # Re-convert to easy-to-handle dfm
      
      fcm_red <- quanteda::as.dfm(fcm) %>% 
        quanteda::dfm_select(unlist(keywords)) 
      
      keywords <- lapply(
        keywords,
        function(i) {
          intersect(i, quanteda::featnames(fcm_red))})
      
      # Find words from the same family as keywords
      
      keywords_derivatives <- lapply(
        
        keywords,
        
        function(i) {
          
          freq_list <- lapply(
            
            i,
            
            function(j) {
              
              derivatives <- unlist(stringr::str_extract_all(
                quanteda::featnames(fcm),
                sprintf("(.?)*(%s)(.?)*", j)))
              
              freq_dt <- as.data.table(
                quanteda::featfreq(fcm)[unlist(derivatives)],
                keep.rownames = TRUE)
              
              data.table::setnames(freq_dt, c("derivative", "freq"))
              data.table::setorder(freq_dt, -freq)
              
            })
          
          freq_list <- do.call(rbind, freq_list)
          
        })
      
      # Find top co-occurring words
      
      keywords_byterms <- lapply(
        
        keywords,
        
        function(i) {
          
          this_fcm <- quanteda::dfm_select(fcm, i)
          this_dt <- convert_qtda_to_dt(this_fcm, key = NULL)
          
          this_dt[
            , names(this_dt)[-1] := lapply(
              .SD,
              function(j) doc_id[order(j, decreasing = TRUE)]), 
            .SDcols = names(this_dt)[-1]
          ][, doc_id := NULL]
          
          data.table::setcolorder(this_dt, i)
          
        })
      
      keywords_byterms_merged <- do.call(cbind, keywords_byterms)
      
      # Remove duplicates
      
      # To make this faster, first prune data to maximum required length 
      # (corresponds to worst case where all keywords and derivatives are 
      # duplicates across topics)
      
      n_derivatives <- sum(sapply(keywords_derivatives, nrow))
      n_potential_dup <- 
        ncol(keywords_byterms_merged) * n_byterms + n_derivatives
      keywords_byterms_short <- keywords_byterms_merged[1:n_potential_dup]
      
      # Remove terms that co-occur with other keywords in higher frequencies
      
      keywords_byterms_unique <- sapply(
        
        seq_along(keywords_byterms_short), 
        
        function(i) {
          
          dup <- private$.find_duplicate_occurrence(keywords_byterms_short)
          keywords_byterms_short[, i, with = FALSE][!dup[, i]]},
        
        USE.NAMES = TRUE)
      
      keywords_byterms_unique <- sapply(
        
        names(keywords),
        
        function(i) {
          
          unname(unlist(keywords_byterms_unique[
            grepl(i, names(keywords_byterms_unique))]))})
      
      # Merge with co-occurrence list and prune to desired length
      
      keywords_list <- lapply(
        
        seq_along(keywords_derivatives),
        
        function(i) {
          
          derivatives <- keywords_derivatives[[i]]$derivative
          byterms <- unlist(keywords_byterms_unique[[i]])
          overlaps <- intersect(derivatives, byterms)
          remainder <- byterms
          
          if (length(overlaps)) {
            remainder <- remainder[!(remainder %in% overlaps)]}
          
          list(
            keyword = names(keywords_derivatives)[i],
            derivatives = derivatives,
            byterms = unname(remainder[1:n_byterms]))})
      
      names(keywords_list) <- names(keywords)
      
      keywords_list
      
    }

  )
)

po_kw <- PipeOpExtractTopicsKeyword$new()
po_kw$param_set$values <- list(
  docid_field = "doc_id",
  text_field = "text",
  keywords = list(
    corona = c("corona", "pandemie", "virus", "krise"),
    klima = c("klima", "grün", "natur", "umwelt")),
  n_byterms = 2L)

graph_preproc <- Graph$new()$add_pipeop(po_kw)

res_preproc <- graph_preproc$train(task)[[1]]
res_preproc$data()

