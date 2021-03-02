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
        ParamUty$new("stopwords"),
        ParamUty$new("keywords"),
        ParamInt$new("n_byterms")
      ))
      
      super$initialize(
        id = id, 
        param_set = ps, 
        param_vals = param_vals,
        packages = c("quanteda", "SnowballC"))
      
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
      
      keywords <- self$param_set$values$keywords
      
      keywords_list <- private$.make_keywords_list(
        keywords = keywords,
        fcm = fcm, 
        n_byterms = self$param_set$values$n_byterms)
      
      if (length(keywords_list) == 0L) {
        return(data.table::copy(dt)[, topic_label := 0L])}

      # Match with keywords

      dict_keywords <- quanteda::dictionary(keywords_list)

      matches_dfm <- quanteda::dfm_lookup(
        dfm,
        dict_keywords,
        levels = 1L:3L)

      matches_dt <- convert_qtda_to_dt(matches_dfm, key = "doc_id")

      # Compute topic label

      matches_topics <- lapply(

        names(keywords_list),

        function(i) {

          relevant_cols <- c(
            "doc_id",
            names(matches_dt)[startsWith(names(matches_dt), i)])
          keyword_cols <- relevant_cols[endsWith(relevant_cols, "keyword")]
          derivative_cols <- relevant_cols[
            endsWith(relevant_cols, "derivatives")]

          dt <- copy(matches_dt)[, ..relevant_cols]

          dt[
            , c(keyword_cols) := lapply(.SD, function(i) 3L * i),
            .SDcols = keyword_cols,
            by = doc_id
            ][, c(derivative_cols) := lapply(.SD, function(i) 2L * i),
              .SDcols = derivative_cols,
              by = doc_id
              ][, sprintf("score_%s", i) := sum(.SD),
                .SDcols = relevant_cols[-1L],
                by = doc_id]})

      matches_topics <- do.call(merge, matches_topics)

      score_cols <- names(matches_topics)[
        startsWith(names(matches_topics), "score")]

      # Prepare output

      matches_topics <- matches_topics[
        , topic_label := ifelse(sum(.SD) == 0L, 0L, which.max(.SD)),
        .SDcols = score_cols,
        by = doc_id
        ][, .(doc_id, topic_label)]

      dt_new <- data.table::copy(dt)[matches_topics, on = "doc_id"]

      dt_new

    },

    .tokenize = function(corpus, stopwords) {

      tkns <- quanteda::tokens(
        corpus,
        what = "word",
        remove_symbols = TRUE,
        remove_numbers = TRUE,
        remove_punct = TRUE,
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
        function(i) sum(data_raw[seq_len(i)] == data_raw[i]))
      
      count_cum_dt <- as.data.table(
        matrix(count_cum, ncol = ncol(data), byrow = TRUE))
      setnames(count_cum_dt, names(data))
      
      count_cum_dt > 1L
      
    },
    
    .make_keywords_list = function(keywords, fcm, n_byterms) {
      
      keywords <- lapply(
        keywords,
        function(i) SnowballC::wordStem(remove_umlauts(i), language = "de"))
      
      if (any(!unlist(keywords) %in% quanteda::featnames(fcm))) {
        warning(sprintf(
          "keyword %s not in data\n",
          unlist(keywords)[
            which(!unlist(keywords) %in% quanteda::featnames(fcm))]))
      }
      
      # Re-convert to easy-to-handle dfm
      
      fcm_red <- quanteda::as.dfm(fcm) %>% 
        quanteda::dfm_select(unlist(keywords)) 
      
      keywords <- lapply(
        keywords,
        function(i) {
          intersect(i, quanteda::featnames(fcm_red))})
      
      if (sum(lengths(keywords)) == 0L) return(list())
      
      keywords <- keywords[lengths(keywords) > 0L]
      
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
            , names(this_dt)[-1L] := lapply(
              .SD,
              function(j) doc_id[order(j, decreasing = TRUE)]), 
            .SDcols = names(this_dt)[-1L]
          ][, doc_id := NULL]
          
          data.table::setcolorder(this_dt, i)
          
        })
      
      keywords_byterms_merged <- do.call(cbind, keywords_byterms)
      
      # Remove duplicates
      
      # To make this faster, first prune data to maximum required length 
      # (corresponds to worst case where all keywords and derivatives are 
      # duplicates across topics)
      
      n_derivatives <- sum(sapply(keywords_derivatives, nrow))
      n_potential_dup <- ncol(keywords_byterms_merged) * n_byterms + 
        n_derivatives
      keywords_byterms_short <- 
        keywords_byterms_merged[seq_len(n_potential_dup)]
      
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
            grepl(i, names(keywords_byterms_unique))]))},
        
        simplify = FALSE)
      
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
            byterms = unname(
              remainder[seq_len(max(length(remainder), n_byterms))]))})
      
      names(keywords_list) <- names(keywords)
      
      keywords_list
      
    }

  )
)
