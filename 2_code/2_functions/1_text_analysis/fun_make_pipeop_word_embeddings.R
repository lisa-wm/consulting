# ------------------------------------------------------------------------------
# WORD EMBEDDINGS PIPEOP
# ------------------------------------------------------------------------------

# PURPOSE: create mlr3pipelines pipe operator for word embeddings

# MAKE PIPEOP ------------------------------------------------------------------

PipeOpMakeGloveEmbeddings = R6::R6Class(
  
  "PipeOpMakeGloveEmbeddings",
  
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  
  public = list(
    
    initialize = function(id = "make_glove_embeddings", param_vals = list()) {
      
      ps = ParamSet$new(params = list(
        ParamUty$new("stopwords", tags = "glove_setup"),
        ParamInt$new("dimension", lower = 1L, tags = "glove_setup"),
        ParamInt$new("term_count_min", lower = 1L, tags = "glove"),
        ParamInt$new("skip_grams_window", lower = 1L, tags = "glove"),
        ParamInt$new("x_max", lower = 1L, tags = "glove"),
        ParamInt$new("n_iter", lower = 1L, tags = "glove"),
        ParamDbl$new("convergence_tol", tags = "glove"),
        ParamInt$new("n_threads", tags = "glove")
      ))
      
      ps$values <- list(
        term_count_min = 1L,
        skip_grams_window = 5L,
        x_max = 10L,
        n_iter = 10L,
        convergence_tol = 1e-02,
        n_threads = 8L)
      
      super$initialize(
        id = id, 
        param_set = ps, 
        param_vals = param_vals,
        packages = c("Matrix", "quanteda", "text2vec"))
      
    }
  ),
  
  private = list(
    
    .train_dt = function(dt, levels, target) {
      
      warn_default <- getOption("warn") 
      options(warn = -1) 
      
      # Subset data by topic label (if present)
      
      if ("topic_label" %in% names(dt)) {
        
        dt_subsets <- lapply(
          sort(unique(dt$topic_label)), 
          function(i) data.table::copy(dt)[topic_label == i])
        
      } else dt_subsets <- list(data.table::copy(dt))

      # Compute embeddings
      
      embeddings <- lapply(
        
        seq_along(dt_subsets),
        
        function(i) {
          
          mlr3misc::invoke(
            private$.make_glove_embeddings,
            .args = c(
              list(
                subset = dt_subsets[[i]], 
                stopwords = self$param_set$values$stopwords,
                dimension = self$param_set$values$dimension), 
              self$param_set$get_values(tags = "glove")))})
      
      self$state <- list(
        terms = lapply(embeddings, function(i) i$terms),
        word_vecs = lapply(embeddings, function(i) i$word_vecs))
      
      # Compute embedding vectors and append
      
      dt_new <- private$.compute_doc_embeddings(
        dt = dt,
        doc_embeddings = lapply(embeddings, function(i) i$doc_embeddings))
      
      options(warn = warn_default)

      dt_new
      
    },
    
    .predict_dt = function(dt, levels) {
      
      warn_default <- getOption("warn") 
      options(warn = -1) 
      
      # Subset data by topic label (if present)
      
      if ("topic_label" %in% names(dt)) {
        
        dt_subsets <- lapply(
          sort(unique(dt$topic_label)), 
          function(i) data.table::copy(dt)[topic_label == i])
        
      } else dt_subsets <- list(data.table::copy(dt))
      
      # Compute embeddings
      
      doc_embeddings <- lapply(
  
        seq_along(dt_subsets),
        
        function(i) {
          
          tkns <- private$.tokenize(
            dt_subsets[[i]]$text, 
            stopwords = self$param_set$values$stopwords)
          
          dtm <- quanteda::dfm_match(
            quanteda::dfm(tkns),
            self$state$terms[[i]])
          
          dtm <- text2vec::normalize(dtm, norm = "l1")
          
          doc_embeddings <- as.matrix(dtm) %*% self$state$word_vecs[[i]]
          
          doc_embeddings <- data.table::as.data.table(doc_embeddings)
          doc_embeddings[, doc_id := dt_subsets[[i]]$doc_id]
          
          })
      
      # Compute embedding vectors and append
      
      dt_new <- private$.compute_doc_embeddings(
        dt = dt,
        doc_embeddings = doc_embeddings)
      
      options(warn = warn_default)
      
      dt_new
        
    },
    
    .tokenize = function(text, stopwords) {
      
      tkns <- quanteda::tokens(
        text,
        what = "word",
        remove_symbols = TRUE,
        remove_punct = TRUE,
        remove_numbers = TRUE,
        remove_separators = TRUE,
        split_hyphens = TRUE,
        include_docvars = TRUE)
      
      tkns <- quanteda::tokens_wordstem(tkns, language = "german")
      
      tkns <- quanteda::tokens_remove(
        quanteda::tokens_tolower(tkns),
        pattern = stopwords)
      
      tkns
      
    },
    
    .make_glove_embeddings = function(subset, 
                                      stopwords,
                                      dimension,
                                      term_count_min,
                                      skip_grams_window,
                                      x_max,
                                      n_iter,
                                      convergence_tol,
                                      n_threads) {
      
      # Tokenize texts and create iterator
      
      tkns <- private$.tokenize(subset$text, stopwords)
      tkns_lst <- as.list(tkns)
      itkns <- text2vec::itoken(tkns_lst, progressbar = FALSE)
      
      # Create (pruned) vocabulary
      
      vcb <- text2vec::create_vocabulary(itkns)
      vcb <- text2vec::prune_vocabulary(vcb, term_count_min = term_count_min)
      
      # Define vectorizer
      
      vect <- text2vec::vocab_vectorizer(vcb)
      
      # Compute co-occurrence matrix
      
      tcm <- text2vec::create_tcm(
        itkns, 
        vect, 
        skip_grams_window = skip_grams_window) 
      
      # Instantiate model
      
      glv <- text2vec::GlobalVectors$new(rank = dimension, x_max = x_max)
      
      # Fit model
      
      wv_main <- glv$fit_transform(
        tcm, 
        n_iter = n_iter, 
        convergence_tol = convergence_tol, 
        n_threads = n_threads)  
      
      # Combine vectors as proposed in original paper
      
      wv_cntxt <- glv$components
      
      word_vecs <- wv_main + t(wv_cntxt)
      
      # Compute document-term matrix
      
      dtm <- quanteda::dfm_match(
        quanteda::dfm(tkns),
        rownames(word_vecs))
      
      # Normalize to achieve row sums of 1 such that the subsequent matrix 
      # multiplication is equivalent to element-wise averaging
      
      dtm <- text2vec::normalize(dtm, norm = "l1")
      
      # Compute document-wise embeddings 
      
      doc_embeddings <- as.matrix(dtm) %*% word_vecs
      
      doc_embeddings <- data.table::as.data.table(doc_embeddings)
      doc_embeddings[, doc_id := subset$doc_id]
      
      list(
        terms = rownames(word_vecs), 
        word_vecs = word_vecs, 
        doc_embeddings = doc_embeddings)
      
    },
    
    .compute_doc_embeddings = function(dt, doc_embeddings) {
      
      # Save doc_id for join
      
      doc_id <- unlist(lapply(doc_embeddings, function(i) i$doc_id))
      invisible(lapply(doc_embeddings, function(i) i[, doc_id := NULL]))
      
      # Create block matrix where each documents' embedding loadings depend
      # on its topic label
      
      doc_embeddings <- lapply(doc_embeddings, as.matrix)
      
      loadings <- do.call(Matrix::bdiag, doc_embeddings)
      
      loadings_dt <- data.table::as.data.table(as.matrix(loadings))
      data.table::setnames(
        loadings_dt, 
        sprintf("embedding_%d", seq_along(loadings_dt)))
      
      # Append back to data
      
      loadings_dt[, doc_id := ..doc_id]
      
      loadings_dt[dt, on = "doc_id"]
      
    }
  )
)
