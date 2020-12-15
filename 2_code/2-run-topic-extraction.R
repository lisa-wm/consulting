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

# Group on per-user, per-month level

# Append electoral and socioeconomic data

data_user_monthly <- make_monthly_user_data(data_processed)

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

data_user_monthly_covariates <- make_covariates(
  tweets_data = data_user_monthly,
  mp_data = data_mp_level,
  se_data = data_socio_electoral
)

# Create corpus object

tweets_corpus_user_monthly <- quanteda::corpus(
  x = data_user_monthly_covariates,
  docid_field = "doc_id",
  text_field = "full_text_user_month")

# Tokenize corpus with custom stopwords

tweets_tokens_user_monthly <- make_tokens(
  corpus = tweets_corpus_user_monthly, 
  stopwords = make_stopwords())

# Create dfm (no weighting, this is not appropriate for stm objects)

tweets_dfm_user_monthly <- make_dfm(
  tokens_ngrams = tweets_tokens_user_monthly,
  min_termfreq = 3L,
  tfidf = FALSE
)

# Create stm object

tweets_stm <- quanteda::convert(
  tweets_dfm_user_monthly, 
  to = "stm")

save(
  tweets_stm,
  file = here("2_code/2_topic_extraction", "rdata-tweets_stm.RData"))

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

hyperparameter_search <- stm::searchK(
  documents = tweets_stm_user_monthly$documents,
  vocab = tweets_stm_user_monthly$vocab,
  data = tweets_stm_user_monthly$meta,
  K = c(3, 4, 5, 6, 7, 8, 9),
  prevalence = prevalence_formula,
  heldout.seed = 123,
  max.em.its = 200,
  init.type = "Spectral"
)

save(
  hyperparameter_search,
  file = here("2_code/2_topic_extraction", "rdata-hyperparameter-search.RData"))

load(here("2_code/2_topic_extraction", "rdata-hyperparameter-search.RData"))