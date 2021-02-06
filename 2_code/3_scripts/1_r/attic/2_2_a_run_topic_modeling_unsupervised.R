# ------------------------------------------------------------------------------
# TOPIC MODELING - UNSUPERVISED
# ------------------------------------------------------------------------------

# IN: dfm object with original documents and topic modeling features
# OUT: corpus object annotated with topic labels + file with topic variables

# GROUP DFM OBJECT BY USER AND MONTH/WEEK --------------------------------------

load_rdata_files(tweets_dfm_tm, folder = "2_code/1_data/2_tmp_data")

# Remove documents where MP has no party or no wahlkreis (STM only works on
# documents to which topical context variables can be assigned)

tweets_dfm_stm <- quanteda::dfm_subset(
  tweets_dfm_tm,
  party != "fraktionslos")

# Aggregate documents in pseudo-documents (original documents are too short for 
# stm to handle, because dfm is very large and sparse)

temporal_grouping_var <- "month"

# TODO check whether dfm should be constrained to terms w/ certain termfreq
# and max docfreq

tweets_dfm_stm_grouped <- quanteda::dfm_group(
  tweets_dfm_stm, 
  c("username", "year", temporal_grouping_var))

# Degree of compression

ndoc(tweets_dfm_stm_grouped) / ndoc(tweets_dfm_stm)
summary(quanteda::ntoken(tweets_dfm_stm))
summary(quanteda::ntoken(tweets_dfm_stm_grouped))

# CREATE STM OBJECT AND DEFINE PREVALENCE FORMULA ------------------------------

# Create stm object (tfidf weighting not possible here)
# Empty documents are dropped

tweets_stm <- quanteda::convert(
  tweets_dfm_stm_grouped,
  to = "stm")

# Define formula for topical prevalence variables (as.formula called outside 
# function so formula is created in global environment)

# TODO find sensible formula

prevalence_formula <- make_prevalence_formula(
  data = quanteda::docvars(tweets_dfm_stm_grouped),
  categorical_vars = list(
    "party", 
    "bundesland"),
  smooth_vars = list(
    "unemployment_rate", 
    "bip_per_capita",
    "share_pop_migration"))

# FIND OR SPECIFY NUMBER OF TOPICS ---------------------------------------------

# Find optimal number of topics - THIS TAKES A WHILE

if (FALSE) {
  
  hyperparameter_search <- stm::searchK(
    documents = tweets_stm$documents,
    vocab = tweets_stm$vocab,
    data = tweets_stm$meta,
    K = c(3L:6L),
    prevalence = prevalence_formula,
    heldout.seed = 1L,
    max.em.its = 1L,
    init.type = "Spectral"
  )
  
  hyperparameter_search_results <- as.data.table(hyperparameter_search$results)
  
  svDialogs::msg_box("done with hyperparameter search")
  
  save_rdata_files(
    hyperparameter_search_results,
    folder = "2_code/1_data/2_tmp_data",
    tmp = FALSE)
  
}

load_rdata_files(
  hyperparameter_search_results, 
  folder = "2_code/1_data/2_tmp_data",
  tmp = FALSE)

n_topics <- as.numeric(hyperparameter_search_results[
  which.min(hyperparameter_search_results[, heldout]), K])

# FIT STM ----------------------------------------------------------------------

if (FALSE) {
  
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
    folder = "2_code/1_data/2_tmp_data",
    tmp = FALSE)
  
}

load_rdata_files(
  topic_model, 
  folder = "2_code/1_data/2_tmp_data",
  tmp = FALSE)

result_tm <- stm::labelTopics(topic_model, n = 50L)
top_words_frex <- t(result_tm$frex)

# ASSIGN TOPIC LABELS TO PSEUDO-DOCUMENTS --------------------------------------

# Convert to data table and remove docnum column (just row number)

topic_probs <- stm::make.dt(topic_model)[
  , `:=` (
    topic_doc_id_stm = names(tweets_stm$documents),
    docnum = NULL)] 

data.table::setnames(topic_probs, tolower(names(topic_probs)))

topic_cols <- sprintf("topic%d", 1:n_topics)

topic_probs[
  , `:=` (
    max_topic_score_stm = max(.SD, na.rm = TRUE),
    topic_label_stm = which.max(.SD)),
  .SDcols = topic_cols,
  by = seq_len(nrow(topic_probs))]

# MAP TOPIC LABLES TO ORIGINAL DOCUMENTS ---------------------------------------

# Extract docvars as data.table for modification (more convenient than quanteda 
# implementation in data.frame format)

docvars_dt <- as.data.table(docvars(tweets_dfm_tm))

# Create ID for pseudo-documents equivalent to what quanteda assigns internally 
# when grouping dfm

docvars_dt[
  , topic_doc_id_stm := sprintf(
    "%s.%d.%d", username, year, get(temporal_grouping_var))]

# Append topic labels

docvars_dt <- topic_probs[docvars_dt, on = "topic_doc_id_stm"]

# For tweets that could not be labeled, choose most frequent topic of author

docvars_dt[
  , top_user_topic_stm := which.max(tabulate(topic_label_stm)), by = username
  ][, topic_label_stm := ifelse(
    is.na(topic_label_stm), 
    top_user_topic_stm, 
    topic_label_stm)
    ][, top_user_topic_stm := NULL
      ][, c(topic_cols) := NULL]

# Insert modified docvars back into corpus

load_rdata_files(tweets_corpus, folder = "2_code/1_data/2_tmp_data")

quanteda::docvars(tweets_corpus) <- as.data.frame(docvars_dt)

save_rdata_files(tweets_corpus, folder = "2_code/1_data/2_tmp_data")
