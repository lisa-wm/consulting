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
data_corpus <- readRDS("../data/topic_preparation/prep_monthly.rds") # data containing raw tweets (before preprocessing)

# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Hyperparameter Search -------------------------------
# ----------------------------------------------------------------------------------------------

# specify model for use of searchK function
covar <- "Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)"
content_var <- "Partei"
outcome <- ""
prevalence <- as.formula(paste(outcome, covar, sep = "~")) 

# # search hyperparameter space for optimal K using searchK function
# hyperparameter_search <- stm::searchK(
#   documents = data$documents,
#   vocab = data$vocab,
#   data = data$meta,
#   K = c(5,10,15,20,25,30,35,40),
#   prevalence = prevalence,
#   heldout.seed = 123,
#   max.em.its = 200,
#   init.type = "Spectral"
# )
# saveRDS(hyperparameter_search, "../data/4/searchK_data.rds")

# load searchK results
searchK_data <- readRDS("../data/4/searchK_data.rds")

# plot four metrics used for hyperparameter search 
plot_heldout <- ggplot(data = searchK_data$results, aes(x = K, y = heldout)) +
  geom_line() +
  geom_point() +
  labs(y = "held-out likelihood") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

plot_semcoh <- ggplot(data = searchK_data$results, aes(x = K, y = semcoh)) + 
  geom_line() +
  geom_point() +
  labs(y = "semantic coherence") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

plot_exclus <- ggplot(data = searchK_data$results, aes(x = K, y = exclus)) +
  geom_line() +
  geom_point() +
  labs(y = "exclusivity") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

plot_residual <- ggplot(data = searchK_data$results, aes(x = K, y = residual)) + 
  geom_line() +
  geom_point() +
  labs(y = "residuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

gridExtra::grid.arrange(plot_heldout, plot_semcoh, plot_exclus, plot_residual, ncol=2)

# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Model Fitting ---------------------------------------
# ----------------------------------------------------------------------------------------------

# # choose covariates and number of topics
# covar <- "Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
#   s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)"
# content_var <- "Partei"
# outcome <- ""
# prevalence <- as.formula(paste(outcome, covar, sep = "~"))
K <- 15

# # fit model
# mod_prev <- stm::stm(
#   documents = data$documents,
#   vocab = data$vocab,
#   data = data$meta,
#   K = K,
#   prevalence = prevalence,
#   gamma.prior = 'L1',
#   seed = 123,
#   max.em.its = 200,
#   init.type = "Spectral")
# saveRDS(mod_prev, "../data/4/mod_prev_monthly.rds")

# load fitted model
mod_prev <- readRDS("../data/4/mod_prev_monthly.rds")

# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Labeling --------------------------------------------
# ----------------------------------------------------------------------------------------------

# labeling workflow (for each topic): 

## (1) inspect most frequent words per topic (using different metrics as well as word cloud)
## (2) evaluate most representative documents (original texts) per topic
## (3) assign label

# first, prepare objects/variables needed for labeling process

## table of MAP topic proportions per document (for all topics)
topic_props <- stm::make.dt(
  mod_prev, 
  data$meta[c("Name", "Partei", "Datum", "Bundesland")]) %>% 
  cbind(docname = names(data$documents), .)

## top words per topic (for all topics)
n <- 5 # number of top words displayed
topic_words <- stm::labelTopics(mod_prev, n = n)

## topic to be evaluated
topic_number <- 1
topic_number_long <- paste0("Topic", topic_number)

## number of top documents to be printed in step (2)
docs_number <- 10

## initialize list with empty labels
topic_labels <- list(
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

# (1)
## top words for all topics
topic_words

## word cloud for selected topic
stm::cloud(mod_prev, topic = topic_number, scale = c(2.0, 0.25))

# (2)
## create variable 'docname' used for matching of raw tweets
data_corpus$docname <- paste0(data_corpus$Twitter_Username, "_", data_corpus$Jahr, "_", data_corpus$Monat)

## match raw tweets with main data
repr_docs <-  topic_props %>%
  arrange(desc(!!as.symbol(topic_number_long))) %>% # order by topic proportion of selected topic, in decreasing order
  .[1:docs_number, c("Name", "docname", "Datum", "Partei", "Bundesland", topic_number_long)] %>% # select variables from main data to be included in output table
  left_join(data_corpus[,c("Tweets_Dokument", "docname")], # select variables from data_corpus to be included
            by = "docname") # specify matching variable

## most representative documents for selected topic
substr(repr_docs$Tweets_Dokument[1], 0, 256) # view most representative document
substr(repr_docs$Tweets_Dokument[2], 0, 254) # view second most representative document

# (3)
topic_labels[[topic_number]] <- "right/nationalist"

# repeat for all K topics
topic_labels <- list(
  Topic1 = "Right/Nationalist",
  Topic2 = "Miscellaneous 1",
  Topic3 = "Climate Economics",
  Topic4 = "Social/Housing",
  Topic5 = "Digital/Future",
  Topic6 = "Climate Protection",
  Topic7 = "Europe",
  Topic8 = "Corona",
  Topic9 = "Left/Anti-war",
  Topic10 = "Twitter/Politics 1",
  Topic11 = "Twitter/Politics 2",
  Topic12 = "Miscellaneous 2",
  Topic13 = "Twitter/Politics 3",
  Topic14 = "Right-wing Extremism",
  Topic15 = "Society/Solidarity"
)

# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Global Topic Proportions ----------------------------
# ----------------------------------------------------------------------------------------------
# determine global topic proportions
## unweighted (simply averaging across all documents)
props_unweighted <- colMeans(mod_prev$theta[,1:K])
## weighted (by number of words per document, i.e., by document length)
doc_lengths <- lapply(data$documents[], length)
weights <- c()
i <- 1
while (i <= length(doc_lengths)) {
  weights[[i]] <- doc_lengths[[i]]/2
  i <- i + 1}
mean_weight <- mean(weights)
props_weighted <- colMeans((mod_prev$theta*weights/mean_weight)[, 1:K])

# prepare plot (unweighted and weighted topic proportions next to each other, for each topic)
topic_labels_unlisted <- unlist(topic_labels, use.names = FALSE)
props_df <- data.frame(topic_labels_unlisted, props_unweighted, props_weighted) %>% reshape2::melt(id = "topic_labels_unlisted")
colnames(props_df) <- c("topic", "variable", "proportion")

# plot
ggplot(data = props_df, aes(x = topic, y = proportion, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("grey40","grey80")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.margin = unit(c(0,0,0,1), "cm"))

# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Global Topic Correlations ---------------------------
# ----------------------------------------------------------------------------------------------

# vocabulary usage comparison for two selected topics
plot(mod_prev, type = "perspectives", topics = c(3, 6), n = 30)

# global topic correlation
## correlation matrix
cormat <- cor(mod_prev$theta)
ggcorrplot::ggcorrplot(cormat) +
  scale_x_continuous(breaks = seq(1, 15, by = 1)) +
  scale_y_continuous(breaks = seq(1, 15, by = 1)) +
  labs(x = "topic number", y = "topic number") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

## topic correlation graph
set.seed(111) # seed chosen for convenient spacing between topic labels
mod_prev_corr <- topicCorr(mod_prev, method = "simple", cutoff = 0.00, verbose = TRUE) # based on correlations between mod_prev$theta
vertex_sizes <- rep(20, K)
stm::plot.topicCorr(mod_prev_corr, vlabels = topic_labels, vertex.label.cex = 1, vertex.size = vertex_sizes)
