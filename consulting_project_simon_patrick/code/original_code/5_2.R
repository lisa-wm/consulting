# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Preparation -----------------------------------------
# ----------------------------------------------------------------------------------------------

# Install and load required packages
os <- Sys.info()[["sysname"]] # Get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # Set corresponding installation type
packages_required <- c(
  "betareg", "ggcorrplot", "grid", "gridExtra", "huge", "knitr", "mvtnorm", 
  "quanteda", "reshape2", "scales", "stm", "stringi", "tidyverse", "tm"
)
not_installed <- packages_required[!packages_required %in%
                                     installed.packages()[, "Package"]]
if (length(not_installed) > 0) {
  lapply(
    not_installed,
    install.packages,
    repos = "http://cran.us.r-project.org",
    dependencies = TRUE,
    type = itype
  )
}
lapply(packages_required, library, character.only = TRUE)

# set working directory (to folder where this code file is saved)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load data
data <- readRDS("../data/topic_preprocessing/preprocessed_monthly.rds")
data_corpus <- readRDS("../data/topic_preparation/prep_monthly.rds")

# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Model Fitting ---------------------------------------
# ----------------------------------------------------------------------------------------------

# choose covariates (now for topical prevalence AND content) and number of topics
covar <- "Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)"
content_var <- "Partei"
outcome <- ""
prevalence <- as.formula(paste(outcome, covar, sep = "~"))
content <- as.formula(paste(outcome, content_var, sep = "~"))

K <- 15

# # fit model
# mod_cont <- stm::stm(
#   documents = data$documents,
#   vocab = data$vocab,
#   data = data$meta,
#   K = K,
#   prevalence = prevalence,
#   content = content,
#   gamma.prior = 'L1',
#   seed = 123,
#   max.em.its = 200,
#   init.type = "Spectral")
# saveRDS(mod_cont, "../data/5_2/mod_cont_monthly.rds")

mod_cont <- readRDS("../data/5_2/mod_cont_monthly.rds")

mod_cont$settings$dim$A # number of parties
K*mod_cont$settings$dim$A # total number of beta-vectors in content model


# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Labeling --------------------------------------------
# ----------------------------------------------------------------------------------------------

# labeling workflow (for each topic): 

## (1) inspect most frequent words per topic (using different metrics as well as word cloud)
## (2) evaluate most representative documents per topic
## (3) assign label

# first, prepare objects/variables needed for labeling process

## table of MAP topic proportions per document (for all topics)
topic_props <- stm::make.dt(
  mod_cont, 
  data$meta[c("Name", "Partei","Datum", "Bundesland")]) %>% 
  cbind(docname = names(data$documents), .)

## top words per topic (for all topics)
n <- 15 # number of top words displayed per topic, per party, and per topic-party interaction
topic_words <- stm::labelTopics(mod_cont, n = n)

## topic to be evaluated
topic_number <- 1
topic_number_long <- paste0("Topic", topic_number)

## number of top documents to be printed in step (2)
docs_number <- 5

## initialize list with empty labels
topic_cont_labels <- list(
  Topic1 = NULL,
  Topic2 = NULL,
  Topic3 = NULL,
  Topic4 = NULL,
  Topic5 = NULL,
  Topic6 = NULL,
  Topic7 = NULL,
  Topic8 = NULL,
  Topic9 = NULL,
  Topic10 = NULL,
  Topic11 = NULL,
  Topic12 = NULL,
  Topic13 = NULL,
  Topic14 = NULL,
  Topic15 = NULL
)

# actual labeling porcess

## (1) inspect most frequent words per topic
topic_words # 20 most frequent words

## (2) evaluate most representative documents per topic
data_corpus$docname <- paste0(data_corpus$Twitter_Username, "_", data_corpus$Jahr, "_", data_corpus$Monat)

repr_docs <-  topic_props %>%
  arrange(desc(!!as.symbol(topic_number_long))) %>%
  .[1:docs_number, c("Name", "docname", "Datum", "Partei", "Bundesland", topic_number_long)] %>%
  left_join(data_corpus[,c("Tweets_Dokument", "docname")], 
            by = "docname")

substr(repr_docs$Tweets_Dokument[1], 0, 256) # view most representative document
topic_number # topic
scales::percent(repr_docs[topic_number_long][1,1], accuracy = 0.01) # proportion
repr_docs$Name[1] # author/MP
repr_docs$Partei[1] # party
repr_docs$Bundesland[1] # state
repr_docs$Datum[1] # date

## (3) assign label
topic_cont_labels[[topic_number]] <- "right/nationalist"

# repeat for all topics
topic_cont_labels <- list(
  Topic1 = "Right/Nationalist 1",
  Topic2 = "Miscellaneous 1",
  Topic3 = "Left/Humanitarian",
  Topic4 = "Housing",
  Topic5 = "Innovation",
  Topic6 = "Green/Energy",
  Topic7 = "Miscellaneous 2",
  Topic8 = "Corona",
  Topic9 = "Foreign Affairs",
  Topic10 = "Election",
  Topic11 = "Right/Nationalist 2",
  Topic12 = "Miscellaneous 3",
  Topic13 = "Miscellaneous 4",
  Topic14 = "Twitter/Politics",
  Topic15 = "Miscellaneous 5"
)

# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Vocabulary Usage across Political Parties -----------
# ----------------------------------------------------------------------------------------------

# pick a topic
topic_number <- 8 # topic number
topic_cont_labels[[topic_number]] # topic label

# show difference in vocabulary usage for two selected political parties for the given topic
plot(mod_cont, type = "perspectives", topics = topic_number,
     covarlevels = c("Bündnis 90/Die Grünen", "AfD"), text.cex = 0.8,
     plabels = c("B'90/Die Grünen", "AfD"))

