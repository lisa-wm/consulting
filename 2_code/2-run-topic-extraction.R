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
  drop = c("district", "wahlkreis"))

# FIXME Contains multiple (non-duplicate) rows per wahlkreis_nr

data_socio_electoral <- data_socio_electoral[
  , head(.SD, 1), by = wahlkreis_nr]

data_user_monthly_covariates <- data_socio_electoral[
  data_mp_level[data_user_monthly, on = "name_matching"],
  on = "wahlkreis_nr"]

# FIXME Change the following in Jupyter

data_user_monthly_covariates[, i.bundesland := NULL]

# FIXME Find out whether asterisks are really resigned MPs

data_user_monthly_covariates <- data_user_monthly_covariates[
  !stringr::str_detect(party, "\\*")
  ][, `:=` (bundesland = as.factor(bundesland), party = as.factor(party))]

levels(data_user_monthly_covariates$party) <- c(
  "afd", "gruene", "cdu_csu", "linke", "fdp", "fraktionslos", "spd"
)

electoral_result <- na.omit(unique(data_user_monthly_covariates[
  , .(wahlkreis_nr, spd, linke, gruene, fdp, afd, cdu_csu)])) %>% 
  gather("party", "vote_share_own_party", -wahlkreis_nr)

data_user_monthly_covariates <- merge(
  data_user_monthly_covariates, 
  electoral_result, 
  by = c("wahlkreis_nr", "party"),
  all.x = TRUE)

data_user_monthly_covariates[
  , time_index := frank(list(year, month), ties.method = "min")
]

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

tweets_stm_user_monthly <- quanteda::convert(
  tweets_dfm_user_monthly, 
  to = "stm")

# STEP 2: DEFINE TOPICAL PREVALENCE VARIABLES ----------------------------------

# Simon and Patrick use: party, state, smooth effect for date, share of 
# immigrant population, GDP, unemployment rate, 2017 electoral result of MPs'
# respective party

# TODO Check whether the model formula is okay for our purposes
# TODO Check whether other covariates, such as covid-19 incidence, should be
# included

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
