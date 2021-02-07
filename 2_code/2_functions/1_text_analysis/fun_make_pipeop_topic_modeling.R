# ------------------------------------------------------------------------------
# CREATION TOPIC MODELING PIPEOP
# ------------------------------------------------------------------------------

# PURPOSE: create mlr3pipelines pipe operator for topic modeling

# MAKE PIPEOP ------------------------------------------------------------------

# to incorporate
# - tokenize
# - convert to dfm
# - group
# - convert to stm
# - run hp search (possibility to specify k upfront)
# - run stm
# - assign topic label

toy_data <- data.table::data.table(cbind(
  iris,
  doc_id = c(seq_len(nrow(iris))),
  full_text = rep("hello What up Pancake Sunday Morning Powder", nrow(iris)),
  username = letters[seq_len(nrow(iris))]))

toy_data <- data_clean[
  , .(doc_id, full_text, username, year, month, party, bundesland, bip_per_capita,
      unemployment_rate, share_pop_migration)
  ][, target := as.factor(sample(c(0, 1), size = nrow(data_clean), replace = TRUE))]

# toy_data <- toy_data[sample(seq_len(nrow(toy_data)), 1000L)]

toy_data <- toy_data[complete.cases(toy_data)]

setattr(
  toy_data$target,
  "levels",
  c("good", "bad"))

task <- mlr3::TaskClassif$new(
  "foo", 
  backend = toy_data,
  target = "target")

PipeOpExtractTopicsSTM = R6::R6Class(
  
  "PipeOpExtractTopics",
  
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  
  public = list(
    
    initialize = function(id = "extract_topics_stm", param_vals = list()) {
      
      ps = ParamSet$new(params = list(
        ParamUty$new("docid_field"),
        ParamUty$new("text_field"),
        ParamUty$new("stm_formula"),
        ParamUty$new("stopwords"),
        ParamUty$new("doc_grouping_var"),
        ParamInt$new("n_topics_lower", lower = 2L),
        ParamInt$new("n_topics_upper", lower = 2L),
        ParamInt$new("max.em.its"),
        ParamFct$new(
          "init.type",
          levels = c("Spectral", "LDA", "Random", "Custom"),
          default = "Spectral"),
        ParamDbl$new("emtol", default = 1e-05),
        ParamDbl$new("seed", default = 1),
        ParamLgl$new("verbose", default = TRUE),
        ParamInt$new("reportevery", default = 5L, lower = 1L),
        ParamLgl$new("LDAbeta", default = TRUE),
        ParamInt$new("ngroups", default = 1L),
        ParamFct$new("gamma.prior", levels = c("Pooled", "L1"), default = "L1"),
        ParamDbl$new("sigma.prior", lower = 0L, upper = 1L, default = 0L),
        ParamFct$new(
          "kappa.prior",
          levels = c("L1", "Jeffreys"),
          default = "L1")
      ))
      
      super$initialize(
        id = id, 
        param_set = ps, 
        param_vals = param_vals,
        packages = c("quanteda", "stm"))
      
    }
  ),
  
  private = list(
    
    .transform_dt = function(dt, levels) {

      # colnames <- c(
      #   self$param_set$values$docid_field, 
      #   self$param_set$values$text_field,
      #   self$param_set$values$doc_grouping_var)
      
      crp <- quanteda::corpus(
        dt,
        # dt[, ..colnames],
        docid_field = self$param_set$values$docid_field,
        text_field = self$param_set$values$text_field)

      tkns <- private$.tokenize(
        corpus = crp, 
        stopwords = self$param_set$values$stopwords)
      
      stm_obj <- private$.make_stm_obj(
        tokens = tkns, 
        doc_grouping_var = self$param_set$values$doc_grouping_var)
      
      stm_mod <- private$.run_stm(
        dt = dt,
        stm = stm_obj,
        stm_formula = as.formula(self$param_set$values$stm_formula),
        k = 3L,
        max.em.its = self$param_set$values$max.em.its)

      # stm_res <- stm::labelTopics(stm_mod, n = 3L)
      # top_words_frex <- t(stm_res$frex)
      
      topic_probs <- stm::make.dt(stm_mod)[
        , `:=` (
          topic_doc_id_stm = names(stm_obj$documents),
          docnum = NULL)]
      
      data.table::setnames(topic_probs, tolower(names(topic_probs)))
      
      topic_cols <- sprintf("topic%d", 1:3)
      
      topic_probs[
        , `:=` (
          max_topic_score_stm = max(.SD, na.rm = TRUE),
          topic_label_stm = which.max(.SD)),
        .SDcols = topic_cols,
        by = seq_len(nrow(topic_probs))]

      dt_new <- cbind(
        dt,
        rep(length(unique(topic_probs$topic_label_stm))))

      setnames(dt_new, letters[1:ncol(dt_new)])
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
      
    },

    # .find_k = function(dt, levels) {dt}, # get_state instead of transform_dt?
   
    .run_stm = function(dt, stm, stm_formula, k, max.em.its) {
      
      stm::stm(
        documents = stm$documents,
        vocab = stm$vocab,
        data = stm$meta,
        K = k,
        prevalence = stm_formula,
        gamma.prior = "L1",
        seed = 1L,
        max.em.its = max.em.its,
        init.type = "Spectral")
      
    }
  )
)

prevalence_formula <- paste(make_prevalence_formula(
  data = data_clean,
  categorical_vars = list(
    "party", 
    "bundesland"),
  smooth_vars = list(
    "unemployment_rate")),
  collapse = "")

pop <- PipeOpExtractTopicsSTM$new()
pop$param_set$values$docid_field <- "doc_id"
pop$param_set$values$text_field <- "full_text"
pop$param_set$values$doc_grouping_var <- c("username", "year", "month")
pop$param_set$values$stm_formula <- prevalence_formula
pop$param_set$values$max.em.its <- 1L
pop$param_set$values$stopwords <- make_stopwords()
gr = Graph$new()$add_pipeop(pop)

result_posa = gr$train(task)[[1]]

result_posa$data()

PipeOpExtractTopicsSTM = R6::R6Class(

  "PipeOpExtractTopics",

  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,

  public = list(

    initialize = function(id = "extract_topics_stm",
                          param_vals = list()) {

      ps = ParamSet$new(params = list(
        ParamUty$new("stm_formula"),
        ParamUty$new("temporal_grouping_var", default = "month"),
        ParamInt$new("n_topics_lower", lower = 1L),
        ParamInt$new("n_topics_upper", lower = 1L),
        ParamInt$new("max_its")
      ))

      super$initialize(id, ps, param_vals = param_vals)

    }
  ),

  private = list(

    # .get_state_dt = function(dt, levels, target) {
    #
    #   list(
    #
    #
    #     # center = sapply(dt, mean),
    #     # scale = 4,
    #     # foo = self$param_set$values$lhs,
    #     # poo = self$param_set$values$rhs
    #   )
    # },

    # alles in get_state wird nur auf trainingsdaten berechnet und dann fuer
    # train und predict verwendet

    .transform_dt = function(dt, levels) {
      coeff <- summary(lm(as.formula(self$param_set$values$stm_formula), data = dt))$coefficients[1]
      dt_new <- cbind(
        dt,
        rep(self$param_set$values$max_its, nrow(dt)),
        # t((t(dt) - self$state$center) / self$state$scale),
        rep(as.numeric(coeff), nrow(dt)))
      setnames(dt_new, letters[1:ncol(dt_new)])
      dt_new
    }
  )
)


pop <- PipeOpExtractTopicsSTM$new()
pop$param_set$values$max_its = 99
pop$param_set$values$stm_formula = "Sepal.Width ~ Sepal.Length"
gr = Graph$new()$add_pipeop(pop)

result_posa = gr$train(task)[[1]]

result_posa$data()
