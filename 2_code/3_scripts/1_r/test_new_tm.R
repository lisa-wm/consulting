{train_data <- tweets_sa[train_set]
train_data <- train_data[complete.cases(train_data)]
test_data <- tweets_sa[test_set]
test_data <- test_data[complete.cases(test_data)]

train_dfm <- quanteda::dfm(
  quanteda::tokens(
    quanteda::corpus(train_data, docid_field = "doc_id", text_field = "text"),
    what = "word",
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    remove_separators = TRUE,
    split_hyphens = TRUE,
    include_docvars = TRUE))

train_dfm_grp <- quanteda::dfm_group(train_dfm, "twitter_username")
train_stm <- quanteda::convert(train_dfm_grp, to = "stm")

test_dfm <- quanteda::dfm(
  quanteda::tokens(
    quanteda::corpus(test_data, docid_field = "doc_id", text_field = "text"),
    what = "word",
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    remove_separators = TRUE,
    split_hyphens = TRUE,
    include_docvars = TRUE))

test_dfm_grp <- quanteda::dfm_group(test_dfm, "twitter_username")
test_stm <- quanteda::convert(test_dfm_grp, to = "stm")

stm_mod <- stm::stm(
  K = 3L,
  max.em.its = 5L,
  documents = train_stm$documents,
  vocab = train_stm$vocab,
  data = train_stm$meta,
  prevalence = ~ meta_party + meta_bundesland)

newdocs <- stm::alignCorpus(new = test_stm, old.vocab = stm_mod$vocab)

foo <- stm::fitNewDocuments(
  stm_mod,
  documents = newdocs$documents,
  newData = newdocs$meta,
  origData = train_stm$meta,
  prevalence = ~ meta_party + meta_bundesland)

foo}



po_tm <- PPP$new()

po_tm$param_set$values <- list(
  K = 3L,
  docid_field = "doc_id",
  text_field = "text",
  doc_grouping_var = c("twitter_username", "twitter_year", "twitter_month"),
  prevalence_vars_cat = list("meta_party", "meta_bundesland"),
  prevalence_vars_smooth = list("meta_unemployment_rate"),
  max.em.its = 5L,
  stopwords = make_stopwords(),
  init.type = "LDA")

ppl_with_tm = Graph$new()$add_pipeop(po_tm)


task <- mlr3::TaskClassif$new(
  "foo",
  backend = train_data,
  target = "label")

foofoo <- ppl_with_tm$train(task$clone())[[1]]
head(foofoo$data())

foo <- ppl_with_tm$predict(task)


PPP = R6::R6Class(
  
  "PPP",
  
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  
  public = list(
    
    initialize = function(id = "foo", param_vals = list()) {
      
      ps = ParamSet$new(params = list(
        ParamUty$new("docid_field", tags = "stm_setup"),
        ParamUty$new("text_field", tags = "stm_setup"),
        ParamUty$new("stopwords", tags = "stm_setup"),
        ParamUty$new("doc_grouping_var", tags = "stm_setup"),
        ParamUty$new("prevalence_vars_cat", tags = "stm_setup"),
        ParamUty$new("prevalence_vars_smooth", tags = "stm_setup"),
        ParamInt$new("K", lower = 2L, tags = "stm"),
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
    
    .train_dt = function(dt, levels, target) {
      
      # Transform data
      
      prevalence_vars <- c(
        unlist(self$param_set$values$prevalence_vars_cat),
        unlist(self$param_set$values$prevalence_vars_smooth))
      
      complete_cases <- dt[, .I][complete.cases(dt[, ..prevalence_vars])]
      
      crp <- quanteda::corpus(
        dt[complete_cases],
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
      
      prevalence_formula <- private$.make_prevalence_formula(
        dt[complete_cases],
        categorical_vars = self$param_set$values$prevalence_vars_cat,
        smooth_vars = self$param_set$values$prevalence_vars_smooth)
      
      stm_mod <- try(
        mlr3misc::invoke(
          stm::stm, 
          .args = c(
            list(
              documents = stm_obj$documents,
              vocab = stm_obj$vocab,
              data = stm_obj$meta,
              prevalence = prevalence_formula),
            self$param_set$get_values(tags = "stm"))))
      
      options(warn = warn_default)
      
      # Compute topic probabilities
      
      if (inherits(stm_mod, "try-error")) {
        
        dt_final <- data.table::copy(dt)[, topic_label := 0L]
        
      } else {
        
        self$state <- list(
          model = stm_mod,
          prevalence_formula = prevalence_formula,
          original_vocab = stm_obj$vocab,
          original_meta = stm_obj$meta)
        
        topic_probs <- stm::make.dt(stm_mod)[
          , `:=` (
            topic_doc_id = names(stm_obj$documents),
            docnum = NULL)]
        
        data.table::setnames(topic_probs, tolower(names(topic_probs)))
        
        topic_cols <- sprintf("topic%d", seq_len(self$param_set$values$K))
        
        topic_probs[
          , topic_label := which.max(.SD),
          .SDcols = topic_cols,
          by = seq_len(nrow(topic_probs))]
        
        # Define output
        
        dt_new <- data.table::copy(dt)
        dt_complete <- dt_new[complete_cases]
        dt_incomplete <- dt_new[setdiff(dt_new[, .I], complete_cases)]
        
        id_cols <- unlist(self$param_set$values$doc_grouping_var)
        
        dt_complete <- dt_complete[
          , topic_doc_id := paste(.SD, collapse = "."),
          .SDcols = id_cols,
          by = seq_len(nrow(dt_complete))]
        
        dt_complete <- topic_probs[dt_complete, on = "topic_doc_id"]
        
        dt_complete[
          , top_user_topic := which.max(tabulate(topic_label)),
          by = get(id_cols[1L])
        ][, topic_label := ifelse(
          is.na(topic_label),
          top_user_topic,
          topic_label)
        ][, top_user_topic := NULL
        ][, c(topic_cols) := NULL
        ][, topic_doc_id := NULL]
        
        dt_incomplete[, topic_label := 0L]
        
        dt_final <- rbind(dt_complete, dt_incomplete)
        data.table::setorder(dt_final, doc_id)
        
      }
      
      dt_final
      
    },
    
    .predict_dt = function(dt, levels) {
      
      # Transform data
      
      prevalence_vars <- c(
        unlist(self$param_set$values$prevalence_vars_cat),
        unlist(self$param_set$values$prevalence_vars_smooth))
      
      complete_cases <- dt[, .I][complete.cases(dt[, ..prevalence_vars])]
      
      crp <- quanteda::corpus(
        dt[complete_cases],
        docid_field = self$param_set$values$docid_field,
        text_field = self$param_set$values$text_field)
      
      tkns <- private$.tokenize(
        corpus = crp, 
        stopwords = self$param_set$values$stopwords)

      stm_obj <- private$.make_stm_obj(
        tokens = tkns, 
        doc_grouping_var = unlist(self$param_set$values$doc_grouping_var))

      stm_obj <- stm::alignCorpus(
        new = stm_obj, 
        old.vocab = self$state$original_vocab)

      topic_probs <- data.table::as.data.table(
        stm::fitNewDocuments(
          model = self$state$model,
          documents = stm_obj$documents,
          newData = stm_obj$meta,
          origData = self$state$original_meta,
          prevalence = self$state$prevalence_formula)$theta)
      
      topic_cols <- sprintf("topic%d", seq_len(ncol(topic_probs)))
      
      data.table::setnames(topic_probs, topic_cols)
      
      topic_probs[, topic_doc_id := names(stm_obj$documents)]
      
      topic_probs[
        , topic_label := which.max(.SD),
        .SDcols = topic_cols,
        by = seq_len(nrow(topic_probs))]
      
      # Define output
      
      dt_new <- data.table::copy(dt)
      dt_complete <- dt_new[complete_cases]
      dt_incomplete <- dt_new[setdiff(dt_new[, .I], complete_cases)]
      
      id_cols <- unlist(self$param_set$values$doc_grouping_var)
      
      dt_complete <- dt_complete[
        , topic_doc_id := paste(.SD, collapse = "."),
        .SDcols = id_cols,
        by = seq_len(nrow(dt_complete))]
      
      dt_complete <- topic_probs[dt_complete, on = "topic_doc_id"]
      
      dt_complete[
        , top_user_topic := which.max(tabulate(topic_label)),
        by = get(id_cols[1L])
      ][, topic_label := ifelse(
        is.na(topic_label),
        top_user_topic,
        topic_label)
      ][, top_user_topic := NULL
      ][, c(topic_cols) := NULL
      ][, topic_doc_id := NULL]
      
      dt_incomplete[, topic_label := 0L]
      
      dt_final <- rbind(dt_complete, dt_incomplete)
      data.table::setorder(dt_final, doc_id)
      
      dt_final
      
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
      
    },
    
    .make_prevalence_formula = function(data, 
                                        categorical_vars, 
                                        smooth_vars,
                                        smooth_df = 5L) {
      
      # Check whether input is of character type
      
      cv <- unlist(categorical_vars)
      sv <- unlist(smooth_vars)
      
      checkmate::assert_character(cv)
      checkmate::assert_character(sv)
      
      # Make sure columns exist in docvars
      
      data_dt <- as.data.table(data)
      cols_docvars <- c(cv, sv)
      
      if (any(!(cols_docvars %in% names(data_dt)))) {
        
        col_not_found <- paste(
          cols_docvars[which(!(cols_docvars %in% names(data_dt)))],
          collapse = ", ")
        
        stop(sprintf("column(s) %s not found in data", col_not_found))
        
      }
      
      cv_formula <- paste0(cv, collapse = "+")
      sv_formula <- paste0("s(", sv, ", df = ", smooth_df, ")", collapse = "+")
      formula_right <- paste0(c(cv_formula, sv_formula), collapse = "+")
      
      as.formula(paste("", formula_right, sep = "~"))
      
    }
    
  )
)
