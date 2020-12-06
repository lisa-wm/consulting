# ------------------------------------------------------------------------------
# PRE-PROCESSING TWEETS
# ------------------------------------------------------------------------------

# Purpose: prepare tweets for SA by creating document-feature-matrices

# Steps:
# 1. Perform basic text cleaning
# 2. Perform lemmatization
# 2. Create document-feature-matrix using mlr3's text pipeop

# STEP 1: PERFORM BASIC TEXT CLEANING ------------------------------------------

# Read data (if retweets are still included, set option to TRUE)

data_raw <- get_data(
  path = here("1_scraping/output", "tweepy_df_subset_no_retweets.csv"),
  is_old_version = FALSE)

# Process tweets in a very basic way - remove umlauts, symbols etc. but keep
# text original otherwise (feature extraction is carried out in step 2);
# convert list and date columns into suitable formats

data_processed <- data_raw %>% 
  make_clean_tweets(column = "full_text") %>% 
  make_clean_meta(
    list_columns = list("hashtags", "mentions"), 
    date_columns = list("created_at"))

data_processed[, word_count := quanteda::ntoken(full_text, remove_punct = TRUE)]

# Save for further analysis

save(
  data_processed, 
  file = here("2_code", "data_processed.RData")
)

# load(here("2_code", "data_processed.RData"))

# STEP 2: PERFORM LEMMATIZATION ------------------------------------------------

# TODO Implement lemmatization (if necessary)

# STEP 3: CREATE DOCUMENT-FEATURE-MATRIX ---------------------------------------

# Conveniently, mlr3 works with quanteda for preprocessing texts, so this step
# can be performed within a pipe operator which can afterwards be fed into a
# graph learner. Simultaneously, the dfm can be extracted for further use in the
# dictionary-based approach.

# Create mlr3 task

# load(here("2_code/0_training_data", "training_data_annotated.RData"))

task <- make_classification_task(
  task_name = "tweets",
  data = training_data_annotated,
  feature_columns = list(
    "full_text", 
    "retweet_count", 
    "favorite_count",
    "followers_count"),
  target_column = "label"
)

save(task, file = here("2_code/1_preprocessing", "task.RData"))

# Create mlr3 graph, where one branch takes care of preprocessing the text
# column while the other just passes the remaining features on

preprocessing_pipeline <- make_preprocessing_pipeline(
  text_column = "full_text",
  ngram = 1L
)

preprocessing_pipeline$plot(html = FALSE)

# Save pipeline to be used for machine learning classifiers

save(
  preprocessing_pipeline,
  file = here("2_code/1_preprocessing", "preprocessing_pipeline.RData"))

# Save output as document-feature-matrix to be used for dictionary-based
# classifier

tweets_dfm_unigram <- preprocessing_pipeline$train(task)[[1]]$data()

save(
  tweets_dfm_unigram,
  file = here("2_code/1_preprocessing", "tweets_dfm_unigram.RData")
)

