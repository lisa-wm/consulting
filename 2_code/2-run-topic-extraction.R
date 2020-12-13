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

parties <- c("total", "spd", "linke", "gruene", "fdp", "afd", "cdu_csu")

setnames(data_user_monthly_covariates, parties, paste0("vote_", parties))

# FIXME Find out whether asterisks are really resigned MPs

data_user_monthly_covariates <- data_user_monthly_covariates[
  !stringr::str_detect(party, "\\*")
  ][, `:=` (bundesland = as.factor(bundesland), party = as.factor(party))]

levels(data_user_monthly_covariates$party) <- c(
  "afd", "gruene", "cdu_csu", "linke", "fdp", "fraktionslos", "spd"
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

# Create dfm

tweets_dfm_user_monthly <- make_dfm(
  tokens_ngrams = tweets_tokens_user_monthly,
  min_termfreq = 1L
)
