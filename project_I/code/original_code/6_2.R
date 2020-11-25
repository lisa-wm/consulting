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

# ----------------------------------------------------------------------------------------------

# load data
data <- readRDS("../data/topic_preprocessing/preprocessed_monthly.rds")
data_train <- readRDS("../data/topic_preprocessing/preprocessed_monthly_train.rds")
data_test <- readRDS("../data/topic_preprocessing/preprocessed_monthly_test.rds")

# choose covariates and number of topics
covar <- "Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)"
outcome <- ""
prevalence <- as.formula(paste(outcome, covar, sep = "~")) 
K <- 15

# ----------------------------------------------------------------------------------------------

# fit model on training data
# mod_train <- stm::stm(
#   documents = data_train$documents,
#   vocab = data_train$vocab,
#   data = data_train$meta,
#   K = K,
#   prevalence = prevalence,
#   gamma.prior = 'L1',
#   seed = 123,
#   max.em.its = 300,
#   init.type = "Spectral"
# )
# saveRDS(mod_train, "../data/6_2/mod_monthly_train.rds")

mod_train <- readRDS("../data/6_2/mod_monthly_train.rds")

# ----------------------------------------------------------------------------------------------

# label topics (according to labeling process; see R-file of section 4)

topic_labels_train <- list(
  Topic1 = "Arms Industry/War",
  Topic2 = "English",
  Topic3 = "Twitter/Politics 1",
  Topic4 = "Twitter/Politics 2",
  Topic5 = "Miscellaneous 1",
  Topic6 = "Right/Nationalist 1",
  Topic7 = "Twitter/Politics 3",
  Topic8 = "Europe",
  Topic9 = "Green/Climate",
  Topic10 = "Income/Taxation",
  Topic11 = "Twitter/Politics 4",
  Topic12 = "Emancipation",
  Topic13 = "Left/Social",
  Topic14 = "Corona",
  Topic15 = "Right/Nationalist 2"
)

# ----------------------------------------------------------------------------------------------

# align corpus
data_test <- stm::alignCorpus(new = data_test, old.vocab = mod_train$vocab, verbose = TRUE)

# fit new documents 
## using option "no prior"
test_none <- stm::fitNewDocuments(
  model = mod_train, 
  documents = data_test$documents, 
  origData = data_train$meta,
  prevalence = prevalence,
  prevalencePrior = "None",
  returnPosterior = FALSE,
  returnPriors = FALSE, 
  designMatrix = NULL, 
  test = TRUE,
  verbose = TRUE
)

# fit new documents 
## using option "average prior"
test_avg <- stm::fitNewDocuments(
  model = mod_train, 
  documents = data_test$documents, 
  origData = data_train$meta,
  prevalence = prevalence,
  prevalencePrior = "Average",
  returnPosterior = FALSE,
  returnPriors = FALSE, 
  designMatrix = NULL, 
  test = TRUE,
  verbose = TRUE
)

# ----------------------------------------------------------------------------------------------

# prepare general dataframes for different data sets
topic_props_train <- make.dt(
  mod_train, data_train$meta[c("Partei", "Bundesland", "Datum", "t", 
                               "Struktur_4", "Struktur_22", "Struktur_42", "Struktur_54")]
)

topic_props_test_none <- make.dt(
  test_none, data_test$meta[c("Partei", "Bundesland", "Datum", "t", "Struktur_4", 
                              "Struktur_22", "Struktur_42", "Struktur_54")]
)

topic_props_test_avg <- make.dt(
  test_avg, data_test$meta[c("Partei", "Bundesland", "Datum", "t", "Struktur_4", 
                             "Struktur_22", "Struktur_42", "Struktur_54")]
)

# ----------------------------------------------------------------------------------------------
# Now, create plots for UN Climate Summit

# filter topic proportions and covariates 
# for time t = 13 and t = 25 (Sep 2018 and Sep 2019 respectively) 
climate_train <- topic_props_train[(t==13) | (t==25)]
climate_train$t <- as.factor(climate_train$t)
climate_train$set <- "Training Set"
climate_test_none <- topic_props_test_none[(t==13) | (t==25)]
climate_test_none$t <- as.factor(climate_test_none$t)
climate_test_none$set <- "Test Set, No Prior"
climate_test_avg <- topic_props_test_avg[(t==13) | (t==25)]
climate_test_avg$t <- as.factor(climate_test_avg$t)
climate_test_avg$set <- "Test Set, Average Prior"
climate_all <- rbind(climate_train, climate_test_none, climate_test_avg)

# create data frame containing mean and credible intervals 
# of topic proportion predictions on respective data set (train/test) with prior none/avg
climate_effects <- data.frame(t = rep(c(13,25), each = 3), 
                              set = rep(c("Training Set", "Test Set, No Prior", "Test Set, Average Prior"),2))
climate_effects$mean[1] <- mean(climate_train[t == climate_effects$t[1], Topic9])
climate_effects$mean[2] <- mean(climate_test_none[t == climate_effects$t[2], Topic9])
climate_effects$mean[3] <- mean(climate_test_avg[t == climate_effects$t[3], Topic9])
climate_effects$mean[4] <- mean(climate_train[t == climate_effects$t[4], Topic9])
climate_effects$mean[5] <- mean(climate_test_none[t == climate_effects$t[5], Topic9])
climate_effects$mean[6] <- mean(climate_test_avg[t == climate_effects$t[6], Topic9])
climate_effects$lower[1] <- quantile(climate_train[t == climate_effects$t[1], Topic9], 0.025)
climate_effects$lower[2] <- quantile(climate_test_none[t == climate_effects$t[2], Topic9], 0.025)
climate_effects$lower[3] <- quantile(climate_test_avg[t == climate_effects$t[3], Topic9], 0.025)
climate_effects$lower[4] <- quantile(climate_train[t == climate_effects$t[4], Topic9], 0.025)
climate_effects$lower[5] <- quantile(climate_test_none[t == climate_effects$t[5], Topic9], 0.025)
climate_effects$lower[6] <- quantile(climate_test_avg[t == climate_effects$t[6], Topic9], 0.025)
climate_effects$upper[1] <- quantile(climate_train[t == climate_effects$t[1], Topic9], 0.975)
climate_effects$upper[2] <- quantile(climate_test_none[t == climate_effects$t[2], Topic9], 0.975)
climate_effects$upper[3] <- quantile(climate_test_avg[t == climate_effects$t[3], Topic9], 0.975)
climate_effects$upper[4] <- quantile(climate_train[t == climate_effects$t[4], Topic9], 0.975)
climate_effects$upper[5] <- quantile(climate_test_none[t == climate_effects$t[5], Topic9], 0.975)
climate_effects$upper[6] <- quantile(climate_test_avg[t == climate_effects$t[6], Topic9], 0.975)

# visualize MAP estimates for both parties and training/test data
climate_effects %>%
  ggplot(aes(x = as.factor(t), y = mean, color = factor(set))) +
  geom_pointrange(aes(ymin = lower, ymax = upper), position=position_dodge(width=0.5)) +
  labs(y = "Topic Proportion", 
       title = "Effect of UN Climate Summit on Topic 'Green/Climate'") +
  ylim(0,1) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(), 
        legend.title=element_blank(), 
        legend.text=element_text(size = 10)) +
  coord_flip() +
  scale_x_discrete(labels=c("September 2018","September 2019"))

# ----------------

# estimate average treatment effect
climate_ATE_train <- mean(climate_train[t == 25, Topic9]) - mean(climate_train[t == 13, Topic9])
climate_ATE_test_none <- mean(climate_test_none[t == 25, Topic9]) - mean(climate_test_none[t == 13, Topic9])
climate_ATE_train_avg <- mean(climate_test_avg[t == 25, Topic9]) - mean(climate_test_avg[t == 13, Topic9])
climate_ATE_df <- data.frame(ATE = c(climate_ATE_train, climate_ATE_test_none, climate_ATE_train_avg), 
                     set = c("Training Set", "Test Set, No Prior", "Test Set, Average Prior"))
# visualize average treatment effect (ATE)
ggplot(climate_ATE_df, aes(set, ATE)) +
  geom_col(width=0.6) +
  labs(y = "Estimated Average Treatment Effect",
       title = "UN Climate Summit - Effect on Topic 'Green/Climate'") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, face = "bold"), 
        legend.text=element_text(size = 10))

# ----------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------
# Finally, create plots for topic 'Emancipation'

# filter topic proportions and covariates 
# for AfD and Green Party
emancipation_train <- topic_props_train[(Partei == "AfD") | (Partei == "Bündnis 90/Die Grünen")]
emancipation_train$set <- "Training Set"
emancipation_test_none <- topic_props_test_none[(Partei == "AfD") | (Partei == "Bündnis 90/Die Grünen")]
emancipation_test_none$set <- "Test Set, No Prior"
emancipation_test_avg <- topic_props_test_avg[(Partei == "AfD") | (Partei == "Bündnis 90/Die Grünen")]
emancipation_test_avg$set <- "Test Set, Average Prior"
emancipation_all <- rbind(emancipation_train, emancipation_test_none, emancipation_test_avg)

# again, create data frame containing mean and credible intervals 
# of topic proportion predictions on respective data set (train/test) with prior none/avg
emancipation_effects <- data.frame(Partei = rep(c("AfD","Bündnis 90/Die Grünen"), each = 3), 
                              set = rep(c("Training Set", "Test Set, No Prior", "Test Set, Average Prior"),2))
emancipation_effects$mean[1] <- mean(emancipation_train[Partei == emancipation_effects$Partei[1], Topic12])
emancipation_effects$mean[2] <- mean(emancipation_test_none[Partei == emancipation_effects$Partei[2], Topic12])
emancipation_effects$mean[3] <- mean(emancipation_test_avg[Partei == emancipation_effects$Partei[3], Topic12])
emancipation_effects$mean[4] <- mean(emancipation_train[Partei == emancipation_effects$Partei[4], Topic12])
emancipation_effects$mean[5] <- mean(emancipation_test_none[Partei == emancipation_effects$Partei[5], Topic12])
emancipation_effects$mean[6] <- mean(emancipation_test_avg[Partei == emancipation_effects$Partei[6], Topic12])
emancipation_effects$lower[1] <- quantile(emancipation_train[Partei == emancipation_effects$Partei[1], Topic12], 0.025)
emancipation_effects$lower[2] <- quantile(emancipation_test_none[Partei == emancipation_effects$Partei[2], Topic12], 0.025)
emancipation_effects$lower[3] <- quantile(emancipation_test_avg[Partei == emancipation_effects$Partei[3], Topic12], 0.025)
emancipation_effects$lower[4] <- quantile(emancipation_train[Partei == emancipation_effects$Partei[4], Topic12], 0.025)
emancipation_effects$lower[5] <- quantile(emancipation_test_none[Partei == emancipation_effects$Partei[5], Topic12], 0.025)
emancipation_effects$lower[6] <- quantile(emancipation_test_avg[Partei == emancipation_effects$Partei[6], Topic12], 0.025)
emancipation_effects$upper[1] <- quantile(emancipation_train[Partei == emancipation_effects$Partei[1], Topic12], 0.975)
emancipation_effects$upper[2] <- quantile(emancipation_test_none[Partei == emancipation_effects$Partei[2], Topic12], 0.975)
emancipation_effects$upper[3] <- quantile(emancipation_test_avg[Partei == emancipation_effects$Partei[3], Topic12], 0.975)
emancipation_effects$upper[4] <- quantile(emancipation_train[Partei== emancipation_effects$Partei[4], Topic12], 0.975)
emancipation_effects$upper[5] <- quantile(emancipation_test_none[Partei == emancipation_effects$Partei[5], Topic12], 0.975)
emancipation_effects$upper[6] <- quantile(emancipation_test_avg[Partei == emancipation_effects$Partei[6], Topic12], 0.975)

# visualize MAP estimates for both parties and training/test data
emancipation_effects %>%
  ggplot(aes(x = as.factor(Partei), y = mean, color = factor(set))) +
  geom_pointrange(aes(ymin = lower, ymax = upper), position=position_dodge(width=0.5)) +
  labs(y = "Topic Proportion", 
       title = "Effect of Poltical Party on Topic 'Emancipation'") +
  ylim(0,1) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(), 
        legend.title=element_blank(), 
        legend.text=element_text(size = 10)) +
  coord_flip()

# ----------------

# estimate average treatment effect
ATE_train <- mean(emancipation_train[Partei == "Bündnis 90/Die Grünen", Topic12]) - mean(emancipation_train[Partei == "AfD", Topic12])
ATE_test_none <- mean(emancipation_test_none[Partei == "Bündnis 90/Die Grünen", Topic12]) - mean(emancipation_test_none[Partei == "AfD", Topic12])
ATE_train_avg <- mean(emancipation_test_avg[Partei == "Bündnis 90/Die Grünen", Topic12]) - mean(emancipation_test_avg[Partei == "AfD", Topic12])
ATE_df <- data.frame(ATE = c(ATE_train, ATE_test_none, ATE_train_avg), 
                     set = c("Training Set", "Test Set, No Prior", "Test Set, Average Prior"))
# visualize average treatment effect (ATE)
ggplot(ATE_df, aes(set, ATE)) +
  geom_col(width=0.6) +
  labs(y = "Estimated Average Treatment Effect",
       title = "AfD vs. Bündnis 90/Die Grünen - Effect on Topic 'Emancipation'") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, face = "bold"), 
        legend.text=element_text(size = 10))

# ----------------------------------------------------------------------------------------------