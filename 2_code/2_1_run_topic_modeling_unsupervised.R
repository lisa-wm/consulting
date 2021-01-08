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

# LOWERCASE TOKENS, REMOVE STOPWORDS AND PERFORM STEMMING

tweets_tokens_tm <- tweets_tokens_tm %>% 
  quanteda::tokens_tolower() %>% 
  quanteda::tokens_select(
    pattern = make_stopwords_tm(),
    selection = "remove"
  ) %>% 
  quanteda::tokens_wordstem(language = "german")

test <- tokens_subset(
  tweets_tokens_tm, 
  as.logical(rbinom(25633, 1, prob = 0.001)))

test <- data.frame(
  embedding_1 = abs(rnorm(10)),
  embedding_2 = abs(rnorm(10)),
  embedding_3 = abs(rnorm(10)),
  embedding_4 = abs(rnorm(10)),
  embedding_5 = abs(rnorm(10)),
  embedding_6 = abs(rnorm(10)),
  embedding_7 = abs(rnorm(10)),
  embedding_8 = abs(rnorm(10)),
  embedding_9 = abs(rnorm(10))
)

test_dfm <- quanteda::as.dfm(test)

test_stm <- quanteda::convert(
  test_dfm,
  to = "stm")

prevalence_covariates <- 
  "party + bundesland + s(time_index, df = 5) + 
  s(1 - pop_german_k / pop_k, df = 5) +
  s(bip_per_capita, df = 5) + s(vote_share_own_party, df = 5)"

prevalence_formula <- as.formula(paste("", prevalence_covariates, sep = "~"))

topic_model <- stm::stm(
  documents = test_stm$documents,
  vocab = test_stm$vocab,
  data = test_stm$meta,
  K = 2,
  # prevalence = prevalence_formula,
  # gamma.prior = 'L1',
  seed = 1L,
  max.em.its = 1L,
  init.type = "Spectral")

stm::labelTopics(topic_model, n = 3L)

sum(sapply(data_clean$hashtags, function(i) length(i) == 0)) / nrow(data_clean)

data_clean[, .N, by = .(year(as.Date(created_at)), username)]
