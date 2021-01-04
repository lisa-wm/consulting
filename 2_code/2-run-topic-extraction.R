# ------------------------------------------------------------------------------
# TOPIC EXTRACTION
# ------------------------------------------------------------------------------

# Purpose: assign topics to tweets

# Steps:

# 1. Create dfm from per-user documents
# 2. Define topical prevalence variables
# 3. 

# STEP 1: CREATE DFM FROM PER-USER DOCUMENTS -----------------------------------

load(here("2_code", "rdata-tweets-processed.RData"))

# Append electoral and socioeconomic data

data_mp_level <- data.table::fread(
  here("1_scraping/output", "abg_twitter_df.csv"),
  encoding = "UTF-8",
  sep = ",",
  drop = "twitter")

data_socio_electoral <- data.table::fread(
  here("1_scraping/output", "socioeconomics_zweitstimmen_df.csv"),
  encoding = "UTF-8",
  sep = ",",
  drop = c("district", "wahlkreis")
)

data_covariates <- append_covariates(
  tweets_data = data_processed,
  mp_data = data_mp_level,
  se_data = data_socio_electoral
)

# Create corpus object

tweets_corpus_stm <- quanteda::corpus(
  x = data_covariates,
  docid_field = "doc_id",
  text_field = "full_text")

# Tokenize corpus with custom stopwords

tweets_tokens_stm <- make_tokens(
  corpus = tweets_corpus_stm, 
  stopwords = make_stopwords())

# Create dfm (no weighting, this is not appropriate for stm objects)

tweets_dfm_stm <- make_dfm(
  tokens_ngrams = tweets_tokens_stm,
  min_termfreq = 3L,
  tfidf = FALSE
)

# Create stm object

tweets_stm <- quanteda::convert(
  tweets_dfm_stm, 
  to = "stm")

save(
  tweets_stm,
  file = here("2_code/2_topic_extraction", "rdata-tweets-stm.RData"))

# STEP 2: DEFINE TOPICAL PREVALENCE VARIABLES ----------------------------------

# Simon and Patrick use: party, state, smooth effect for date, share of 
# immigrant population, GDP, unemployment rate, 2017 electoral result of MPs'
# respective party

load(here("2_code/2_topic_extraction", "rdata-tweets_stm.RData"))

# TODO Check whether the model formula is okay for our purposes
# TODO Check whether other covariates, such as covid-19 incidence, should be
# included

# For this, none of the covariates may contain NAs!

prevalence_covariates <- 
  "party + bundesland + s(time_index, df = 5) + 
  s(1 - pop_german_k / pop_k, df = 5) +
  s(bip_per_capita, df = 5) + s(vote_share_own_party, df = 5) + s(afd, df = 5)"

prevalence_formula <- as.formula(paste("", prevalence_covariates, sep = "~"))

# STEP 3: DETERMINE NUMBER OF TOPICS -------------------------------------------

# Attention, hyperparameter search takes long, therefore commented out

# hyperparameter_search <- stm::searchK(
#   documents = tweets_stm_user_monthly$documents,
#   vocab = tweets_stm_user_monthly$vocab,
#   data = tweets_stm_user_monthly$meta,
#   K = c(3, 4, 5, 6, 7, 8, 9),
#   prevalence = prevalence_formula,
#   heldout.seed = 123,
#   max.em.its = 200,
#   init.type = "Spectral"
# )
# 
# save(
#   hyperparameter_search,
#   file = here(
#     "2_code/2_topic_extraction", 
#     "rdata-hyperparameter-search.RData"))

load(here("2_code/2_topic_extraction", "rdata-hyperparameter-search.RData"))

n_topics <- 8L

# STEP 4: RUN TOPIC MODEL ------------------------------------------------------

# topic_model_k_8 <- stm::stm(
#   documents = tweets_stm$documents,
#   vocab = tweets_stm$vocab,
#   data = tweets_stm$meta,
#   K = n_topics,
#   prevalence = prevalence_formula,
#   gamma.prior = 'L1',
#   seed = 1L,
#   max.em.its = 20L,
#   init.type = "Spectral")

topic_model_k_8_new <- stm::stm(
  documents = tweets_stm_new$documents,
  vocab = tweets_stm_new$vocab,
  data = tweets_stm_new$meta,
  K = n_topics,
  prevalence = prevalence_formula,
  gamma.prior = 'L1',
  seed = 1L,
  max.em.its = 10L,
  init.type = "Spectral")

# STEP 5: LABEL TWEETS WITH TOPICS ---------------------------------------------

# topic_props <- stm::make.dt(
#   topic_model_k_8, 
#   tweets_stm$meta[c(
#     "username", 
#     "last_name", 
#     "first_name", 
#     "party", 
#     "year", 
#     "month", 
#     "bundesland")]) %>% 
#   cbind(docname = names(tweets_stm$documents), .)

# Convert topic model to data.table containing selected meta data columns and 
# topic proportions

topic_props_new <- stm::make.dt(
  topic_model_k_8_new, 
  tweets_stm_new$meta[c(
    "username", 
    "last_name", 
    "first_name", 
    "party", 
    "created_at",
    "bundesland")]) %>% 
  cbind(doc_id = names(tweets_stm_new$documents), .)

setnames(topic_props_new, tolower(names(topic_props_new)))

# Append label for topic with maximum score

topic_cols <- paste0("topic", 1:n_topics)

topic_props_new[
  , `:=` (
    max_topic_score = max(.SD, na.rm = TRUE),
    max_topic = which.max(.SD)),
  .SDcols = topic_cols,
  by = seq_len(nrow(topic_props_new))
]


