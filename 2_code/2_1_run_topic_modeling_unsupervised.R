# ------------------------------------------------------------------------------
# TOPIC MODELING - UNSUPERVISED
# ------------------------------------------------------------------------------

# IN: single data file of cleaned tweets and meta data
# OUT: single data file of cleaned tweets and meta data with topic label + 
# file of topic-word distributions

# CONVERT TO TOKENS OBJECT -----------------------------------------------------

load(
  here(
    "2_code", 
    paste0("rdata_", as.character(bquote(tweets_corpus)), ".RData")))

tweets_tokens_tm <- quanteda::tokens(
  tweets_corpus,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_separators = TRUE,
  split_hyphens = TRUE,
  include_docvars = TRUE) 

# LOWERCASE TOKENS, REMOVE STOPWORDS AND PERFORM STEMMING ----------------------

# Standard stopwords removal and stemming

tweets_tokens_tm <- tweets_tokens_tm %>% 
  quanteda::tokens_wordstem(language = "german") %>% 
  quanteda::tokens_select(
    pattern = c("[[:upper:]]([[:lower:]])+"),
    valuetype = "regex",
    selection = "keep",
    case_insensitive = FALSE) %>% 
  quanteda::tokens_tolower() %>%
  quanteda::tokens_select(
    pattern = make_stopwords_tm(),
    selection = "remove") 

# CREATE DFM OBJECT AND GROUP DOCUMENTS BY USER AND MONTH/WEEK -----------------

# Create dfm 

tweets_dfm_tm <- quanteda::dfm(tweets_tokens_tm)

# Aggregate documents in pseudo-documents fit for topic modeling (original 
# documents are too short, so dfm is very large and sparse)

temporal_grouping_var <- "month"

tweets_dfm_tm_grouped <- quanteda::dfm_group(
  tweets_dfm_tm, c("username", "year", temporal_grouping_var))

# Degree of compression

ndoc(tweets_dfm_tm_grouped) / ndoc(tweets_dfm_tm)
summary(ntoken(tweets_dfm_tm))
summary(ntoken(tweets_dfm_tm_grouped))

# CREATE STM OBJECT AND FIT STM ------------------------------------------------

# Create stm object

tweets_stm <- quanteda::convert(
  tweets_dfm_tm_grouped,
  to = "stm")

# Define formula for topical prevalence variables (as.formula called outside 
# function so formula is created in global environment)
# TODO find sensible formula

prevalence_formula <- as.formula(make_prevalence_formula(
  data = docvars(tweets_dfm_tm_grouped),
  categorical_vars = list(
    "party", 
    "bundesland"),
  smooth_vars = list(
    "unemployment_rate", 
    "bip_per_capita",
    "share_pop_migration")))

# Find optimal number of topics - THIS TAKES A WHILE

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

n_topics <- as.numeric(hyperparameter_search_results[
  which.min(hyperparameter_search_results[, heldout]), K])

# Fit STM

topic_model <- stm::stm(
  documents = tweets_stm$documents,
  vocab = tweets_stm$vocab,
  data = tweets_stm$meta,
  K = n_topics,
  prevalence = prevalence_formula,
  gamma.prior = 'L1',
  seed = 1L,
  max.em.its = 10L,
  init.type = "Spectral")

result_tm <- stm::labelTopics(topic_model, n = 15L)

top_words_frex <- t(result_tm$frex)

# ASSIGN TOPIC LABELS TO PSEUDO-DOCUMENTS --------------------------------------

topic_probs <- stm::make.dt(topic_model)[, `:=` (
  topic_doc_id = names(tweets_stm$documents),
  docnum = NULL)] 

setnames(topic_probs, tolower(names(topic_probs)))

topic_cols <- paste0("topic", 1:n_topics)

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

tweets_corpus_topics <- tweets_corpus
docvars(tweets_corpus_topics) <- as.data.frame(docvars_dt)

save(
  tweets_corpus_topics, 
  file = here(
    "2_code", 
    paste0("rdata_", as.character(bquote(tweets_corpus_topics)), ".RData")))
