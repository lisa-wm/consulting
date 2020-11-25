# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Preparation -----------------------------------------
# ----------------------------------------------------------------------------------------------

# Install and load required packages
os <- Sys.info()[["sysname"]] # Get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # Set corresponding installation type
devtools::install_github("PMSchulze/stmprevalence") # install from github
packages_required <- c(
  "grid", "gridExtra", "scales", "ggplot2", "bayesplot", "rstanarm", 
  "stm", "stmprevalence", "tidyverse"
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
theme_set(bayesplot::theme_default())

# set working directory (to folder where this code file is saved)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ----------------------------------------------------------------------------------------------

# load data
data <- readRDS("../data/topic_preprocessing/preprocessed_monthly.rds")
# load fitted topic model
mod_prev <- readRDS("../data/4/mod_prev_monthly.rds")
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
  "Unemployement Rate (%)", "Vote share (%)"
)
# select topics 1,4,6 and covariates
formula <- c(1,4,6)~Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)
metadata <- data$meta[varlist]
# factorize
metadata[sapply(metadata, is.character)] <- lapply(metadata[sapply(metadata, is.character)], 
                                                   as.factor)

# ----------------------------------------------------------------------------------------------
# --------------------------------------- Calculations -----------------------------------------
# ----------------------------------------------------------------------------------------------

# # Obtain MAP estimates of nsims Bayesian beta regressions
# mod_betaregs <- stmprevalence::beta_bayes(mod_prev, formula, metadata, nsims = 25)
# 
# # Draw from posterior predictive distribution of previously obtained Beta regressions
# all_betas_bayes <- stmprevalence::posterior_predict_props(mod_betaregs, "t", formula, 
#                                                       metadata, 0.025, 0.975)
# # Store sampled values
# saveRDS(all_betas_bayes, "../data/5_1/all_betas_bayes.rds")

# load previously calculated results for Bayesian beta regression
preds_beta_bayes <- readRDS("../data/5_1/all_betas_bayes.rds")

# ----------------------------------------------------------------------------------------------
# ------------------------------------------ Plotting ------------------------------------------
# ----------------------------------------------------------------------------------------------

## Topic 6: Climate Protection
### Continuous plot without credible intervals
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_betabayes_", v)
  assign(plot_nam, ggplot(preds_beta_bayes[[v]]$Topic6, aes(x = !!as.symbol(v), y = proportion)) +
           geom_smooth(color = "black", method = "loess", se = FALSE, size = 0.8) +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) + 
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text=element_text(size=12), 
                 axis.title.x = element_text(size=16)))
}
### Change axis labeling for time effects
ticks_date <- data.frame(breaks = c(1,13,25), labels = 0)
for (i in ticks_date$breaks) ticks_date[ticks_date["breaks"]==i, "labels"] <- 
  (data$meta$Datum[which(data$meta$t == i)] %>% unique)
plot_betabayes_Date <- plot_betabayes_t + 
  scale_x_continuous(name = "Date", breaks = ticks_date$breaks, labels = ticks_date$labels)
### Combine all plots
gridExtra::grid.arrange(plot_betabayes_Date, plot_betabayes_Struktur_4, 
                        plot_betabayes_Struktur_22, plot_betabayes_Struktur_42, ncol=2, 
                        top = grid::textGrob("Topic 6: Climate Protection", 
                                             gp=grid::gpar(fontsize=16, fontface = "bold")))

## Topic 6: Climate Protection
### Continuous plot with credible intervals
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_betabayes_", v)
  plot_smoothed_ci <- ggplot(preds_beta_bayes[[v]]$Topic6) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_lower), method = "loess", se = FALSE) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_upper), method = "loess", se = FALSE)
  smoothed_ci <- ggplot_build(plot_smoothed_ci)
  df_smoothed_ci <- data.frame(v = smoothed_ci$data[[1]]$x,
                               ci_lower = smoothed_ci$data[[1]]$y,
                               ci_upper = smoothed_ci$data[[2]]$y)
  assign(plot_nam, plot_smoothed_ci + 
           geom_ribbon(data = df_smoothed_ci, aes(x = v, ymin = ci_lower, ymax = ci_upper), 
                       fill = "grey70") +
           geom_smooth(data = preds_beta_bayes[[v]]$Topic6, aes(x = !!as.symbol(v), y = proportion),
                       color = "black", method = "loess", se = FALSE, size = 0.8) +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) + 
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text=element_text(size=12), 
                 axis.title.x = element_text(size=16)))
}
### Change axis labeling for time effects
ticks_date <- data.frame(breaks = c(1,13,25), labels = 0)
for (i in ticks_date$breaks) ticks_date[ticks_date["breaks"]==i, "labels"] <- 
  (data$meta$Datum[which(data$meta$t == i)] %>% unique)
plot_betabayes_Date <- plot_betabayes_t + 
  scale_x_continuous(name = "Date", breaks = ticks_date$breaks, labels = ticks_date$labels)
### Combine all plots
gridExtra::grid.arrange(plot_betabayes_Date, plot_betabayes_Struktur_4, 
                        plot_betabayes_Struktur_22, plot_betabayes_Struktur_42, ncol=2, 
                        top = grid::textGrob("Topic 6: Climate Protection", 
                                             gp=grid::gpar(fontsize=16, fontface = "bold")))

## Topic 6: Climate Protection
### Categorial plot with credible intervals
(plot_betabayes_party_6 <- ggplot(preds_beta_bayes$Partei$Topic6, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic 6: Climate Protection")+
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
          axis.title.x = element_text(size=16)))


# For topic 4, we show our plots in appendix

## Topic 4: Social/Housing
### Continuous plot without credible intervals
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_betabayes_", v)
  assign(plot_nam, ggplot(preds_beta_bayes[[v]]$Topic4, aes(x = !!as.symbol(v), y = proportion)) +
           geom_smooth(color = "black", method = "loess", se = FALSE, size = 0.8) +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) + 
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text=element_text(size=12), 
                 axis.title.x = element_text(size=16)))
}
### Change axis labeling for time effects
ticks_date <- data.frame(breaks = c(1,13,25), labels = 0)
for (i in ticks_date$breaks) ticks_date[ticks_date["breaks"]==i, "labels"] <- 
  (data$meta$Datum[which(data$meta$t == i)] %>% unique)
plot_betabayes_Date <- plot_betabayes_t + 
  scale_x_continuous(name = "Date", breaks = ticks_date$breaks, labels = ticks_date$labels)
### Combine all plots
gridExtra::grid.arrange(plot_betabayes_Date, plot_betabayes_Struktur_4, 
                        plot_betabayes_Struktur_22, plot_betabayes_Struktur_42, ncol=2, 
                        top = grid::textGrob("Topic 4: Social/Housing", 
                                             gp=grid::gpar(fontsize=16, fontface = "bold")))

## Topic 4: Social/Housing
### Continuous plot with credible intervals
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_betabayes_", v)
  plot_smoothed_ci <- ggplot(preds_beta_bayes[[v]]$Topic4) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_lower), method = "loess", se = FALSE) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_upper), method = "loess", se = FALSE)
  smoothed_ci <- ggplot_build(plot_smoothed_ci)
  df_smoothed_ci <- data.frame(v = smoothed_ci$data[[1]]$x,
                               ci_lower = smoothed_ci$data[[1]]$y,
                               ci_upper = smoothed_ci$data[[2]]$y)
  assign(plot_nam, plot_smoothed_ci + 
           geom_ribbon(data = df_smoothed_ci, aes(x = v, ymin = ci_lower, ymax = ci_upper), 
                       fill = "grey70") +
           geom_smooth(data = preds_beta_bayes[[v]]$Topic4, aes(x = !!as.symbol(v), y = proportion),
                       color = "black", method = "loess", se = FALSE, size = 0.8) +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) + 
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text=element_text(size=12), 
                 axis.title.x = element_text(size=16)))
}
### Change axis labeling for time effects
ticks_date <- data.frame(breaks = c(1,13,25), labels = 0)
for (i in ticks_date$breaks) ticks_date[ticks_date["breaks"]==i, "labels"] <- 
  (data$meta$Datum[which(data$meta$t == i)] %>% unique)
plot_betabayes_Date <- plot_betabayes_t + 
  scale_x_continuous(name = "Date", breaks = ticks_date$breaks, labels = ticks_date$labels)
### Combine all plots
gridExtra::grid.arrange(plot_betabayes_Date, plot_betabayes_Struktur_4, 
                        plot_betabayes_Struktur_22, plot_betabayes_Struktur_42, ncol=2, 
                        top = grid::textGrob("Topic 4: Social/Housing", 
                                             gp=grid::gpar(fontsize=16, fontface = "bold")))

## Topic 4: Social/Housing
### Categorial plot with credible intervals
(plot_betabayes_party_4 <- ggplot(preds_beta_bayes$Partei$Topic4, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic 4: Social/Housing")+
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
          axis.title.x = element_text(size=16)))
