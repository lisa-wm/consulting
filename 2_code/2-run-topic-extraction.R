# ------------------------------------------------------------------------------
# TOPIC EXTRACTION
# ------------------------------------------------------------------------------

# Purpose: assign topics to tweets

# Steps:

# 1. Create stm object
# 2. Define topical prevalence variables
# 3. 

# STEP 1: CREATE STM OBJECT ----------------------------------------------------

load(here("2_code", "rdata-tweets-dfm-unigrams.RData"))

# Create stm object from sample of dfm (too large otherwise)

tweets_dfm_sampled <- quanteda::dfm_sample(
  tweets_dfm_unigrams,
  size = 
)

tweets_stm <- quanteda::convert(
  tweets_dfm_unigrams,
  to = "stm")

save(
  tweets_stm,
  file = here("2_code/2_topic_extraction", "rdata-tweets-stm.RData"))

# STEP 2: DEFINE TOPICAL PREVALENCE VARIABLES ----------------------------------

# Simon and Patrick use: party, state, smooth effect for date, share of 
# immigrant population, GDP, unemployment rate, 2017 electoral result of MPs'
# respective party

# TODO Check whether the model formula is okay for our purposes
# TODO Check whether other covariates, such as covid-19 incidence, should be
# included

# For this, none of the covariates may contain NAs (and may not be of type list)

prevalence_covariates <- 
  "party + bundesland + s(time_index, df = 5) + 
  s(1 - pop_german_k / pop_k, df = 5) +
  s(bip_per_capita, df = 5) + s(vote_share_own_party, df = 5)"

prevalence_covariates <- 
  "party + bundesland + s(time_index, df = 5) + 
  s(1 - pop_german_k / pop_k, df = 5) +
  s(bip_per_capita, df = 5) + s(vote_share_own_party, df = 5)"

prevalence_formula <- as.formula(paste("", prevalence_covariates, sep = "~"))

# STEP 3: DETERMINE NUMBER OF TOPICS -------------------------------------------

# Attention, hyperparameter search takes long, therefore commented out

# FIXME Throws error bc vector is too large (did not happen for monthly tweets)

hyperparameter_search <- stm::searchK(
  documents = tweets_stm$documents,
  vocab = tweets_stm$vocab,
  data = tweets_stm$meta,
  K = c(3:10),
  prevalence = prevalence_formula,
  heldout.seed = 1L,
  max.em.its = 1L,
  init.type = "Spectral"
)

save(
  hyperparameter_search,
  file = here(
    "2_code/2_topic_extraction",
    "rdata-hyperparameter-search.RData"))

load(here("2_code/2_topic_extraction", "rdata-hyperparameter-search.RData"))

hyperparameter_search_results <- as.data.table(hyperparameter_search$results)

n_topics <- as.numeric(hyperparameter_search_results[
  which.min(hyperparameter_search_results[, heldout]), K
])

n_topics <- 8L

# STEP 4: RUN TOPIC MODEL ------------------------------------------------------

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

top_words <- stm::labelTopics(topic_model, n = 10L)

# STEP 5: LABEL TWEETS WITH TOPICS ---------------------------------------------

# Convert topic model to data.table containing selected meta data columns and 
# topic proportions

topic_props <- stm::make.dt(topic_model)[, `:=` (
  doc_id = names(tweets_stm$documents),
  docnum = NULL)] 
setnames(topic_props, tolower(names(topic_props)))

# Append label for topic with maximum score

topic_cols <- paste0("topic", 1:n_topics)

topic_props[
  , `:=` (
    max_topic_score = max(.SD, na.rm = TRUE),
    max_topic = which.max(.SD)),
  .SDcols = topic_cols,
  by = seq_len(nrow(topic_props))
]


