# ------------------------------------------------------------------------------
# TOPIC MODELING PIPEOP (STM)
# ------------------------------------------------------------------------------

# PURPOSE: create mlr3pipelines pipe operator for topic modeling with STM

# MAKE PIPEOP ------------------------------------------------------------------

PipeOpExtractTopicsSTM = R6::R6Class(
  
  "PipeOpExtractTopicsSTM",
  
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  
  public = list(
    
    initialize = function(id = "extract_topics_stm", param_vals = list()) {
      
      ps = ParamSet$new(params = list(
        ParamUty$new("docid_field"),
        ParamUty$new("text_field"),
        ParamUty$new("stopwords"),
        ParamUty$new("doc_grouping_var"),
        ParamUty$new("prevalence", tags = "stm"),
        ParamInt$new("K", lower = 2L, tags = "stm"),
        # ParamInt$new("n_topics_lower", lower = 2L),
        # ParamInt$new("n_topics_upper", lower = 2L),
        ParamInt$new("max.em.its", tags = "stm"),
        ParamFct$new(
          "init.type",
          levels = c("Spectral", "LDA", "Random", "Custom"),
          tags = "stm"),
        ParamDbl$new("emtol", tags = "stm"),
        ParamDbl$new("seed", tags = "stm"),
        ParamLgl$new("verbose", tags = "stm"),
        ParamInt$new("reportevery", lower = 1L, tags = "stm"),
        ParamLgl$new("LDAbeta", tags = "stm"),
        ParamInt$new("ngroups", tags = "stm"),
        ParamFct$new("gamma.prior", levels = c("Pooled", "L1"), tags = "stm"),
        ParamDbl$new("sigma.prior", lower = 0L, upper = 1L, tags = "stm"),
        ParamFct$new("kappa.prior", levels = c("L1", "Jeffreys"), tags = "stm")
      ))
      
      ps$values <- list(
        init.type = "Spectral",
        emtol = 1e-05,
        seed = 123L,
        verbose = FALSE,
        reportevery = 5L,
        LDAbeta = TRUE,
        ngroups = 1L,
        gamma.prior = "L1",
        sigma.prior = 0L,
        kappa.prior = "L1")
      
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
      
      stm_obj <- private$.make_stm_obj(
        tokens = tkns, 
        doc_grouping_var = unlist(self$param_set$values$doc_grouping_var))
      
      # Run topic model
      
      warn_default <- getOption("warn") 
      options(warn = -1) 
      
      stm_mod <- mlr3misc::invoke(
        stm::stm, 
        .args = c(
          list(
            documents = stm_obj$documents,
            vocab = stm_obj$vocab,
            data = stm_obj$meta),
          self$param_set$get_values(tags = "stm")))
      
      options(warn = warn_default)
      
      # Compute topic probabilities
      
      topic_probs <- stm::make.dt(stm_mod)[
        , `:=` (
          topic_doc_id = names(stm_obj$documents),
          docnum = NULL)]
      
      data.table::setnames(topic_probs, tolower(names(topic_probs)))
      
      topic_cols <- sprintf("topic%d", seq_len(self$param_set$values$K))
      
      topic_probs[
        , `:=` (
          max_topic_score = max(.SD, na.rm = TRUE),
          topic_label = which.max(.SD)),
        .SDcols = topic_cols,
        by = seq_len(nrow(topic_probs))]
      
      # Define output
 
      dt_new <- data.table::copy(dt)
      
      id_cols <- unlist(self$param_set$values$doc_grouping_var)
      
      dt_new <- dt_new[
        , topic_doc_id := paste(.SD, collapse = "."),
        .SDcols = id_cols,
        by = seq_len(nrow(dt_new))]
      
      dt_new <- topic_probs[dt_new, on = "topic_doc_id"]
      
      dt_new[
        , top_user_topic := which.max(tabulate(topic_label)),
        by = get(id_cols[1L])
        ][, topic_label := ifelse(
          is.na(topic_label),
          top_user_topic,
          topic_label)
          ][, top_user_topic := NULL
            ][, c(topic_cols) := NULL
              ][, topic_doc_id := NULL]
      
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

    .make_stm_obj = function(tokens, doc_grouping_var) {
      
      dfm <- quanteda::dfm(tokens)
      dfm_grp <- quanteda::dfm_group(dfm, doc_grouping_var)
      quanteda::convert(dfm_grp, to = "stm")
      
    }

    # .find_k = function(dt, levels) {dt}, # get_state instead of transform_dt?
    
    # FIXME find way to compute k

  )
)
