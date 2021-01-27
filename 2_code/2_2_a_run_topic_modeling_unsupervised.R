# ------------------------------------------------------------------------------
# TOPIC MODELING - UNSUPERVISED
# ------------------------------------------------------------------------------

# IN: dfm object with original documents and topic modeling features
# OUT: corpus object annotated with topic labels + file with topic variables

# GROUP DFM OBJECT BY USER AND MONTH/WEEK --------------------------------------

load_rdata_files(tweets_dfm_tm, folder = "2_code")

# Aggregate documents in pseudo-documents (original documents are too short for 
# stm to handle, because dfm is very large and sparse)

temporal_grouping_var <- "month"

tweets_dfm_tm_grouped <- quanteda::dfm_group(
  tweets_dfm_tm, c("username", "year", temporal_grouping_var))

# Degree of compression

ndoc(tweets_dfm_tm_grouped) / ndoc(tweets_dfm_tm)
summary(quanteda::ntoken(tweets_dfm_tm))
summary(quanteda::ntoken(tweets_dfm_tm_grouped))

# CREATE STM OBJECT AND DEFINE PREVALENCE FORMULA ------------------------------

# Create stm object

tweets_stm <- quanteda::convert(
  tweets_dfm_tm_grouped,
  to = "stm")

# Define formula for topical prevalence variables (as.formula called outside 
# function so formula is created in global environment)
# TODO find sensible formula

prevalence_formula <- as.formula(make_prevalence_formula(
  data = quanteda::docvars(tweets_dfm_tm_grouped),
  categorical_vars = list(
    "party", 
    "bundesland"),
  smooth_vars = list(
    "unemployment_rate", 
    "bip_per_capita",
    "share_pop_migration")))

# FIND OR SPECIFY NUMBER OF TOPICS ---------------------------------------------

# Find optimal number of topics - THIS TAKES A WHILE

run_hyperparameter_search <- readline(
  prompt = paste0(
    "Would you like to re-run a time-intensive search for optimal k? ",
    "T/F: "))

if (as.logical(run_hyperparameter_search)) {
  
  hyperparameter_search <- stm::searchK(
    documents = tweets_stm$documents,
    vocab = tweets_stm$vocab,
    data = tweets_stm$meta,
    K = c(3L:6L),
    prevalence = prevalence_formula,
    heldout.seed = 1L,
    max.em.its = 5L,
    init.type = "Spectral"
  )
  
  hyperparameter_search_results <- as.data.table(hyperparameter_search$results)
  
  svDialogs::msg_box("done with hyperparameter search")
  
  save_rdata_files(
    hyperparameter_search_results,
    folder = "2_code/2_topic_extraction")
  
}

load_rdata_files(
  hyperparameter_search_results, 
  folder = "2_code/2_topic_extraction")

n_topics <- as.numeric(hyperparameter_search_results[
  which.min(hyperparameter_search_results[, heldout]), K])

# FIT STM ----------------------------------------------------------------------

run_stm <- readline(
  prompt = paste0(
    "Would you like to re-run the STM (time-intensive)? ", 
    "T/F: "))

if (run_stm) {
  
  topic_model <- stm::stm(
    documents = tweets_stm$documents,
    vocab = tweets_stm$vocab,
    data = tweets_stm$meta,
    K = n_topics,
    prevalence = prevalence_formula,
    gamma.prior = 'L1',
    seed = 1L,
    max.em.its = 15L,
    init.type = "Spectral")
  
  svDialogs::msg_box("done fitting stm")
  
  save_rdata_files(
    topic_model,
    folder = "2_code/2_topic_extraction")
  
}

load_rdata_files(
  topic_model, 
  folder = "2_code/2_topic_extraction")

result_tm <- stm::labelTopics(topic_model, n = 50L)
top_words_frex <- t(result_tm$frex)

# ASSIGN TOPIC LABELS TO PSEUDO-DOCUMENTS --------------------------------------

topic_probs <- stm::make.dt(topic_model)[, `:=` (
  topic_doc_id = names(tweets_stm$documents),
  docnum = NULL)] 

data.table::setnames(topic_probs, tolower(names(topic_probs)))

topic_cols <- sprintf("topic_%d", 1:n_topics)

topic_probs[
  , `:=` (
    max_topic_score = max(.SD, na.rm = TRUE),
    topic_label = which.max(.SD)),
  .SDcols = topic_cols,
  by = seq_len(nrow(topic_probs))
]

# MAP TOPIC LABLES TO ORIGINAL DOCUMENTS ---------------------------------------

# Extract docvars as data.table for modification (more convenient than quanteda 
# implementation in data.frame format)

docvars_dt <- as.data.table(docvars(tweets_dfm_tm))

# Create ID for pseudo-documents equivalent to what quanteda assigns internally 
# when grouping dfm

docvars_dt[, topic_doc_id := paste0(
  username, ".", year, ".", get(temporal_grouping_var))]

# Append topic labels

docvars_dt <- topic_probs[docvars_dt, on = "topic_doc_id"]

# For tweets that could not be labeled, choose most frequent topic of author

docvars_dt[
  , top_user_topic := which.max(tabulate(topic_label)), by = username
  ][, topic_label := ifelse(is.na(topic_label), top_user_topic, topic_label)]

# Insert modified docvars back into corpus

tweets_corpus_topics_unsupervised <- tweets_corpus
quanteda::docvars(tweets_corpus_topics_unsupervised) <- 
  as.data.frame(docvars_dt)

save_rdata_files(
  robject = tweets_corpus_topics_unsupervised, 
  folder = "2_code")
