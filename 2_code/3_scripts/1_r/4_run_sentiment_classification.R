# ------------------------------------------------------------------------------
# SENTIMENT CLASSIFICATION PIPELINE
# ------------------------------------------------------------------------------

# IN: data filtered for subjective tweets
# OUT: data with sentiment labels

data <- tweets_subjective[
  label != "none"
  ][, created_at := NULL]

# cols_to_keep <- names(data)[c(1:54, 57:58, 104, 124:128)]
# data <- data[, ..cols_to_keep]

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
  max.em.its = 1L,
  stopwords = make_stopwords(),
  K = 3L)

graph <- Graph$new()$add_pipeop(po_tm)

result_tm <- graph$train(task)[[1]]
result_tm$data()

po_cart <- PipeOpLearner$new(learner = lrn("classif.rpart"))

po_sel <- PipeOpSelect$new()
po_sel$param_set$values$selector <- 
  selector_invert(selector_type(c("character")))

graph <- graph %>>% po_sel %>>% po_cart

plot(graph, html = F)

graph_learner <- GraphLearner$new(graph)

graph_learner$train(task)
res <- graph_learner$predict(task)
res$confusion

result_tm <- graph$train(task)[[1]]
result_tm$data()

po_ge <- PipeOpMakeGloveEmbeddings$new()
po_ge$param_set$values$dimension <- 3L
po_ge$param_set$values$stopwords <- make_stopwords()
graph <- Graph$new()$add_pipeop(po_ge)

po_sel2 <- PipeOpSelect$new()
po_sel2$param_set$values$selector <- selector_name(c("topic_label", "text"))

graph <- Graph$new()$add_pipeop(po_tm)
graph <- graph %>>% po_sel2 %>>% po_ge

result_ge <- graph$train(task)[[1]]
result_ge$data()
