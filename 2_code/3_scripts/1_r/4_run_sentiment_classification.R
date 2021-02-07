# ------------------------------------------------------------------------------
# SENTIMENT CLASSIFICATION PIPELINE
# ------------------------------------------------------------------------------

# IN: data filtered for subjective tweets
# OUT: data with sentiment labels

data <- tweets_subjective[
  label != "none"
  ][, created_at := NULL]

data$label <- as.factor(data$label)

task <- mlr3::TaskClassif$new(
  "twitter_ttsa", 
  backend = data,
  target = "label")

prevalence_formula <- make_prevalence_formula(
  data = task$data(),
  categorical_vars = list(
    "party", 
    "bundesland"),
  smooth_vars = list(
    "unemployment_rate"))

po_tm <- PipeOpExtractTopicsSTM$new()

po_tm$param_set$values <- list(
  docid_field = "doc_id",
  text_field = "text",
  doc_grouping_var = c("username", "year", "month"),
  prevalence = prevalence_formula,
  max.em.its = 5L,
  stopwords = make_stopwords(),
  K = 3L)

# result_tm <- graph$train(task)[[1]]
# result_tm$data()

po_sel_ge <- PipeOpSelect$new(id = "select_embedding")
po_sel_ge$param_set$values$selector <- selector_name(c("topic_label", "text"))
po_sel_ge_inv <- PipeOpSelect$new(id = "select_rest")
po_sel_ge_inv$param_set$values$selector <- 
  selector_invert(selector_name(c("topic_label", "text")))

po_ge <- PipeOpMakeGloveEmbeddings$new()
po_ge$param_set$values$dimension <- 3L
po_ge$param_set$values$stopwords <- make_stopwords()

po_nop <- PipeOpNOP$new()

po_sel_cl <- PipeOpSelect$new(id = "select_classif")
po_sel_cl$param_set$values$selector <- 
  selector_invert(selector_type(c("character")))

po_cart <- PipeOpLearner$new(learner = lrn("classif.rpart"))

graph <- Graph$new()$add_pipeop(po_tm)

graph <- graph %>>% 
  gunion(list(
    po_sel_ge %>>% 
      po_ge,
    po_sel_ge_inv %>>% 
      po_nop)
    ) %>>%
  po("featureunion") 

plot(graph, html = FALSE)

# res_preproc <- graph$train(task)[[1]]
# res_preproc$data()

graph_full <- graph %>>%
  po_sel_cl %>>%
  po_cart

graph_learner <- GraphLearner$new(graph_full)

graph_learner$train(task)
res <- graph_learner$predict(task)
res$confusion
