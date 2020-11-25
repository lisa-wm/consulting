# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Preparation -----------------------------------------
# ----------------------------------------------------------------------------------------------

# Install and load required packages
os <- Sys.info()[["sysname"]] # Get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # Set corresponding installation type
packages_required <- c(
  "betareg", "ggcorrplot", "grid", "gridExtra", "huge", "knitr", "mvtnorm", 
  "quanteda", "reshape2", "scales", "stm", "stmprevalence", "stringi", "tidyverse", "tm"
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

# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Model Fitting ---------------------------------------
# ----------------------------------------------------------------------------------------------

# choose number of topics
K <- 15

# # fit model (no prevalence structure, thus also no gamma prior)
# mod_ctm <- stm::stm(
#   documents = data$documents,
#   vocab = data$vocab,
#   data = data$meta,
#   K = K,
#   # prevalence = prevalence,
#   # gamma.prior = 'L1',
#   seed = 123,
#   max.em.its = 300,
#   init.type = "Spectral")
# saveRDS(mod_ctm, "../data/6_1/mod_ctm_monthly.rds")

mod_ctm <- readRDS("../data/6_1/mod_ctm_monthly.rds")

# load topic labels
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

# load list of prevalence covariates
varlist <- c(
  "t", "Partei", "Bundesland", "Struktur_4", "Struktur_22", "Struktur_42", "Struktur_54"
)
# load full names of prevalence covariates
varlist_fullnames <- c(
  "Time", "Party", "Federal State", "Immigrants (%)", "GDP per capita", 
  "Unemployement Rate (%)", "vote share (%)"
)

formula <- 1:15~Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)


# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Comparison with STM ---------------------------------
# ----------------------------------------------------------------------------------------------

# load STM
mod_prev <- readRDS("../data/4/mod_prev_monthly.rds")

# load prevalence structure of STM
covar <- "Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) +
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)"
outcome <- ""
prevalence <- as.formula(paste(outcome, covar, sep = "~"))

# comparison of heldout likelihoods
## prepare dataframe of heldout documents
heldout <- stm::make.heldout(
  documents = data$documents,
  vocab = data$vocab,
  N = floor(0.1 * length(data$documents)), # hold out words from 10% of all documents
  proportion = 0.5, # for each of the documents within these 10%, hold out 50% of all words
  seed = 123)

## train STM and CTM on non-heldout documents
# mod_prev_heldout <- stm::stm(
#                       documents = heldout$documents,
#                       vocab = heldout$vocab,
#                       data = data$meta,
#                       K = K,
#                       prevalence = prevalence,
#                       gamma.prior = 'L1',
#                       seed = 123,
#                       max.em.its = 300,
#                       init.type = "Spectral")
# saveRDS(mod_prev_heldout, "../data/6_1/mod_prev_heldout_monthly.rds")
mod_prev_heldout <- readRDS("../data/6_1/mod_prev_heldout_monthly.rds")

# mod_ctm_heldout <- stm::stm(
#                       documents = heldout$documents,
#                       vocab = heldout$vocab,
#                       data = data$meta,
#                       K = K,
#                       # prevalence = prevalence,
#                       # gamma.prior = 'L1',
#                       seed = 123,
#                       max.em.its = 300,
#                       init.type = "Spectral")
# saveRDS(mod_ctm_heldout, "../data/6_1/mod_ctm_heldout_monthly.rds")
mod_ctm_heldout <- readRDS("../data/6_1/mod_ctm_heldout_monthly.rds")

## compare heldout likelihoods of STM and CTM
stm::eval.heldout(mod_prev_heldout, heldout$missing)[["expected.heldout"]] # heldout likelihood of STM (model with prevalence)
stm::eval.heldout(mod_ctm_heldout, heldout$missing)[["expected.heldout"]] # heldout likelihood of CTM (model without prevalence)

# comparison of global topic proportions
diff_matrix <- mod_prev$theta - mod_ctm$theta # matrix of differences between (document-level) topic proportions
abs_diff_matrix <- abs(diff_matrix) # absolute differences

scales::percent(mean(rowMeans(abs_diff_matrix)), accuracy = 0.01) # average absolute difference in topic proportions per topic, averaged across all documents
scales::percent(mean(abs(colMeans(diff_matrix))), accuracy = 0.01) # average absolute difference in global (!) topic proportions per topic (topic proportions simply averaged across all documents)

# ----------------------------------------------------------------------------------------------
# ---------------------- Plots with stmprevalence: Method of Composition -----------------------
# ----------------------------------------------------------------------------------------------

# # estimate 100 beta regressions and sample from regressions coefficients
# all_betas_ctm <- sample_coefs(mod_ctm, formula = formula, type = "beta",
#                             data$meta, nsims = 100, seed = 123)
# # estimate 100 quasibinomial glms and sample from regressions coefficients
# all_quasibin_ctm <- sample_coefs(mod_ctm, formula = formula, type = "quasibinomial",
#                             data$meta, nsims = 100, seed = 123)
# # save results
# saveRDS(all_betas_ctm, "../data/6_1/all_betas_ctm.rds")
# saveRDS(all_quasibin_ctm, "../data/6_1/all_quasibin_ctm.rds")

# load previously computed results
all_betas <- readRDS("../data/6_1/all_betas_ctm.rds")
all_quasibin <- readRDS("../data/6_1/all_quasibin_ctm.rds")
# predict thetas using beta regression for all variables
preds_beta <- lapply(varlist, 
                     function(v) stmprevalence::predict_props(all_betas, v, formula, data$meta))
# predict thetas using quasibinomial glm for all variables
preds_quasibin <- lapply(varlist, 
                         function(v) stmprevalence::predict_props(all_quasibin, v, formula, data$meta))
names(preds_beta) <- names(preds_quasibin) <- varlist

# ----------------------------------------------------------------------------------------------

# First, we create all plots using the qusibinomial GLM; these plots are shown in main section

## Topic 6: Climate Protection -- Quasibinomial GLM
### Continuous Plots
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_quasibin_", v)
  assign(plot_nam, ggplot(preds_quasibin[[v]]$Topic6, aes(!!as.symbol(v))) + 
           geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey70") +
           xlab(varlist_fullnames[varlist==v]) +
           ylab("Expected Topic Proportion") +
           geom_line(aes(y = proportion)) +
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text=element_text(size=12), 
                 axis.title.x = element_text(size=16)))
}
### Change axis labeling for time effects
ticks_date <- data.frame(breaks = c(1,13,25), labels = 0)
for (i in ticks_date$breaks) ticks_date[ticks_date["breaks"]==i, "labels"] <- 
  (data$meta$Datum[which(data$meta$t == i)] %>% unique)
plot_quasibin_Date <- plot_quasibin_t + 
  scale_x_continuous(name = "Date", breaks = ticks_date$breaks, labels = ticks_date$labels)
### Combine all plots
gridExtra::grid.arrange(
  plot_quasibin_Date, plot_quasibin_Struktur_4, plot_quasibin_Struktur_22, plot_quasibin_Struktur_42, ncol=2, 
  top = grid::textGrob("Topic 6: Climate Protection", gp=grid::gpar(fontsize=16, fontface = "bold"))
)

## Topic 6: Climate Protection -- Quasibinomial GLM
### Categorical Plots
(plot_party_6 <- ggplot(preds_quasibin$Partei$Topic6, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic 6: Climate Protection") +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
          axis.title.x = element_text(size=16)))

## Topic 4: Social/Housing -- Quasibinomial GLM
### Continuous Plots
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_quasibin_", v)
  assign(plot_nam, ggplot(preds_quasibin[[v]]$Topic4, aes(!!as.symbol(v))) + 
           geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey70") +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) +
           geom_line(aes(y = proportion)) +
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text=element_text(size=12), 
                 axis.title.x = element_text(size=16)))
}
### Change axis labeling for time effects
ticks_date <- data.frame(breaks = c(1,13,25), labels = 0)
for (i in ticks_date$breaks) ticks_date[ticks_date["breaks"]==i, "labels"] <- 
  (data$meta$Datum[which(data$meta$t == i)] %>% unique)
plot_quasibin_Date <- plot_quasibin_t + 
  scale_x_continuous(name = "Date", breaks = ticks_date$breaks, labels = ticks_date$labels)
### Combine all plots
gridExtra::grid.arrange(
  plot_quasibin_Date, plot_quasibin_Struktur_4, plot_quasibin_Struktur_22, plot_quasibin_Struktur_42, 
  ncol=2, 
  top = grid::textGrob("Topic 4: Social/Housing", gp=grid::gpar(fontsize=16, fontface = "bold"))
)

## Topic 4: Social/Housing -- Quasibinomial GLM
### Categorical Plots
(plot_party_4 <- ggplot(preds_quasibin$Partei$Topic4, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic 4: Social/Housing")+
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
          axis.title.x = element_text(size=16)))

## Topics 1,6, and 4 -- Quasibinomial GLM
### Categorical Plots
preds_quasibin$Partei$Topic1$Topic <- "Right/Nationalist"
preds_quasibin$Partei$Topic6$Topic <- "Climate Protection"
preds_quasibin$Partei$Topic4$Topic <- "Social/Housing"
party_data <- rbind(preds_quasibin$Partei$Topic1, preds_quasibin$Partei$Topic6, 
                    preds_quasibin$Partei$Topic4)
(plot_party <- ggplot(party_data, aes(y=proportion, x = Partei, fill = Topic)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values=c("#53A567FF", "#56A8CBFF", "#DA291CFF")) +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic Proportions by Party") +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

# ----------------------------------------------------------------------------------------------

# Then, we create all plots using the beta regression; these plots are shown in appendix

## Topic 6: Climate Protection -- Beta regression
### Continuous Plots
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_beta_", v)
  assign(plot_nam, ggplot(preds_beta[[v]]$Topic6, aes(!!as.symbol(v))) + 
           geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey70") +
           xlab(varlist_fullnames[varlist==v]) +
           ylab("Expected Topic Proportion") +
           geom_line(aes(y = proportion)) +
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text=element_text(size=12), 
                 axis.title.x = element_text(size=16)))
}
### Change axis labeling for time effects
ticks_date <- data.frame(breaks = c(1,13,25), labels = 0)
for (i in ticks_date$breaks) ticks_date[ticks_date["breaks"]==i, "labels"] <- 
  (data$meta$Datum[which(data$meta$t == i)] %>% unique)
plot_beta_Date <- plot_beta_t + 
  scale_x_continuous(name = "Date", breaks = ticks_date$breaks, labels = ticks_date$labels)
### Combine all plots
gridExtra::grid.arrange(
  plot_beta_Date, plot_beta_Struktur_4, plot_beta_Struktur_22, plot_beta_Struktur_42, ncol=2, 
  top = grid::textGrob("Topic 6: Climate Protection", gp=grid::gpar(fontsize=16, fontface = "bold"))
)

## Topic 6: Climate Protection -- Beta regression
### Categorical Plots
(plot_party_6 <- ggplot(preds_beta$Partei$Topic6, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic 6: Climate Protection") +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
          axis.title.x = element_text(size=16)))

## Topic 4: Social/Housing -- Beta regression
### Continuous Plots
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_beta_", v)
  assign(plot_nam, ggplot(preds_beta[[v]]$Topic4, aes(!!as.symbol(v))) + 
           geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey70") +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) +
           geom_line(aes(y = proportion)) +
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text=element_text(size=12), 
                 axis.title.x = element_text(size=16)))
}
### Change axis labeling for time effects
ticks_date <- data.frame(breaks = c(1,13,25), labels = 0)
for (i in ticks_date$breaks) ticks_date[ticks_date["breaks"]==i, "labels"] <- 
  (data$meta$Datum[which(data$meta$t == i)] %>% unique)
plot_beta_Date <- plot_beta_t + 
  scale_x_continuous(name = "Date", breaks = ticks_date$breaks, labels = ticks_date$labels)
### Combine all plots
gridExtra::grid.arrange(
  plot_beta_Date, plot_beta_Struktur_4, plot_beta_Struktur_22, plot_beta_Struktur_42, ncol=2, 
  top = grid::textGrob("Topic 4: Social/Housing", gp=grid::gpar(fontsize=16, fontface = "bold"))
)

## Topic 4: Social/Housing -- Beta regression
### Categorical Plots
(plot_party_4 <- ggplot(preds_beta$Partei$Topic4, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic 4: Social/Housing")+
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
          axis.title.x = element_text(size=16)))

## Topics 1,6, and 4 -- Beta regression
### Categorical Plots
preds_beta$Partei$Topic1$Topic <- "Right/Nationalist"
preds_beta$Partei$Topic6$Topic <- "Climate Protection"
preds_beta$Partei$Topic4$Topic <- "Social/Housing"
party_data <- rbind(preds_beta$Partei$Topic1, preds_beta$Partei$Topic6, preds_beta$Partei$Topic4)
(plot_party <- ggplot(party_data, aes(y=proportion, x = Partei, fill = Topic)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values=c("#53A567FF", "#56A8CBFF", "#DA291CFF")) +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic Proportions by Party") +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))
