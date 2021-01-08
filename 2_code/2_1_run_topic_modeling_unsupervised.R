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

tweets_dfm_tm <- quanteda::dfm(tweets_tokens_tm)

temporal_grouping_var <- "month"

tweets_dfm_tm_grouped <- quanteda::dfm_group(
  tweets_dfm_tm, c("username", "year", temporal_grouping_var))

# CREATE STM OBJECT AND FIT STM ------------------------------------------------

# Create stm object

tweets_stm <- quanteda::convert(
  tweets_dfm_tm_grouped,
  to = "stm")

# Define formula for topical prevalence variables
# TODO find sensible formula

prevalence_covariates <- 
  "party + bundesland +  
  s(1 - pop_german_k / pop_k, df = 5) +
  s(bip_per_capita, df = 5)"

prevalence_formula <- as.formula(paste("", prevalence_covariates, sep = "~"))

# Find optimal number of topics

# hyperparameter_search <- stm::searchK(
#   documents = tweets_stm$documents,
#   vocab = tweets_stm$vocab,
#   data = tweets_stm$meta,
#   K = c(3:10),
#   prevalence = prevalence_formula,
#   heldout.seed = 1L,
#   max.em.its = 1L,
#   init.type = "Spectral"
# )
# 
# hyperparameter_search_results <- as.data.table(hyperparameter_search$results)
# 
# n_topics <- as.numeric(hyperparameter_search_results[
#   which.min(hyperparameter_search_results[, heldout]), K])

n_topics <- 5L

# Fit STM

topic_model <- stm::stm(
  documents = tweets_stm$documents,
  vocab = tweets_stm$vocab,
  data = tweets_stm$meta,
  K = n_topics,
  prevalence = prevalence_formula,
  gamma.prior = 'L1',
  seed = 1L,
  max.em.its = 1L,
  init.type = "Spectral")

result_tm <- stm::labelTopics(topic_model, n = 15L)

# Assign topic labels to documents

topic_probs <- stm::make.dt(topic_model)[, `:=` (
  topic_doc_id = names(tweets_stm$documents),
  docnum = NULL)] 

setnames(topic_probs, tolower(names(topic_probs)))

topic_cols <- paste0("topic", 1:n_topics)

topic_probs[
  , `:=` (
    max_topic_score = max(.SD, na.rm = TRUE),
    max_topic = which.max(.SD)),
  .SDcols = topic_cols,
  by = seq_len(nrow(topic_probs))
]

# Map topic labels to original tweets

tweets_corpus_topics <- tweets_corpus

docvars(tweets_corpus_topics, "topic_doc_id") <- paste0(
  docvars(tweets_corpus_topics, "username"),
  ".",
  docvars(tweets_corpus_topics, "year"),
  ".",
  docvars(tweets_corpus_topics, temporal_grouping_var)
)

docvars(tweets_corpus_topics) <- docvars(tweets_corpus_topics) %>% 
  left_join(
    topic_probs,
    by = "topic_doc_id")

# For tweets that could not be labeled, choose most frequent topic of author

topic_highscore <- docvars(tweets_corpus_topics) %>% 
  group_by(username) %>% 
  count(max_topic) %>% 
  slice(which.max(n)) %>% 
  select(username, max_topic) %>% 
  rename("top_user_topic" = max_topic)

docvars(tweets_corpus_topics) <- docvars(tweets_corpus_topics) %>% 
  left_join(
    topic_highscore[, c("username", "top_user_topic")],
    by = "username") 

docvars(tweets_corpus_topics) <- docvars(tweets_corpus_topics) %>% 
  mutate(topic_label = ifelse(
    is.na(max_topic), 
    top_user_topic,
    max_topic))

save(
  tweets_corpus_topics, 
  file = here(
    "2_code", 
    paste0("rdata_", as.character(bquote(tweets_corpus_topics)), ".RData")))