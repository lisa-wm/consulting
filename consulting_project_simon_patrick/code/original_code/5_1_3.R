# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Preparation -----------------------------------------
# ----------------------------------------------------------------------------------------------

# Install and load required packages
os <- Sys.info()[["sysname"]] # Get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # Set corresponding installation type
devtools::install_github("PMSchulze/stmprevalence") # install from github
packages_required <- c(
  "grid", "gridExtra", "scales", "stmprevalence", "tidyverse"
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

formula <- 1:15~Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)

# ----------------------------------------------------------------------------------------------
# ------------------- Plots with stmprevalence: Direct assessment ------------------------------
# ----------------------------------------------------------------------------------------------

# # Sample from LogisticNormal and calculate mean and credible intervals; 
# # pre-calculated for speed reasons
# preds_logisticn <- lapply(varlist, function(v) sample_props_logisticn(mod_prev, v, formula, data$meta))
# names(preds_logisticn) <- varlist
# saveRDS(preds_logisticn, "../data/5_1/all_logisticn.rds")

# load previously calculated samples from LogisticNormal
preds_logisticn <- readRDS("../data/5_1/all_logisticn.rds")

## Topic 6: Climate Protection
### Continuous plot without credible intervals
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_logisticn_", v)
  assign(plot_nam, ggplot(preds_logisticn[[v]]$Topic6, aes(x = !!as.symbol(v), y = proportion)) +
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
plot_logisticn_Date <- plot_logisticn_t + 
  scale_x_continuous(name = "Date", breaks = ticks_date$breaks, labels = ticks_date$labels)
### Combine all plots
gridExtra::grid.arrange(plot_logisticn_Date, plot_logisticn_Struktur_4, 
                        plot_logisticn_Struktur_22, plot_logisticn_Struktur_42, ncol=2, 
                        top = grid::textGrob("Topic 6: Climate Protection", 
                                             gp=grid::gpar(fontsize=16, fontface = "bold")))

## Topic 6: Climate Protection
### Continuous plot with credible intervals
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_logisticn_", v)
  plot_smoothed_ci <- ggplot(preds_logisticn[[v]]$Topic6) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_lower), method = "loess", se = FALSE) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_upper), method = "loess", se = FALSE)
  smoothed_ci <- ggplot_build(plot_smoothed_ci)
  df_smoothed_ci <- data.frame(v = smoothed_ci$data[[1]]$x,
                               ci_lower = smoothed_ci$data[[1]]$y,
                               ci_upper = smoothed_ci$data[[2]]$y)
  assign(plot_nam, plot_smoothed_ci + 
           geom_ribbon(data = df_smoothed_ci, aes(x = v, ymin = ci_lower, ymax = ci_upper), 
                       fill = "grey70") +
           geom_smooth(data = preds_logisticn[[v]]$Topic6, aes(x = !!as.symbol(v), y = proportion),
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
plot_logisticn_Date <- plot_logisticn_t + 
  scale_x_continuous(name = "Date", breaks = ticks_date$breaks, labels = ticks_date$labels)
### Combine all plots
gridExtra::grid.arrange(plot_logisticn_Date, plot_logisticn_Struktur_4, 
                        plot_logisticn_Struktur_22, plot_logisticn_Struktur_42, ncol=2, 
                        top = grid::textGrob("Topic 6: Climate Protection", 
                                             gp=grid::gpar(fontsize=16, fontface = "bold")))

## Topic 6: Climate Protection
### Categorical plot with credible intervals
(plot_logisticn_party_6 <- ggplot(preds_logisticn$Partei$Topic6, aes(y=proportion, x = Partei)) +
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
  plot_nam <- paste0("plot_logisticn_", v)
  assign(plot_nam, ggplot(preds_logisticn[[v]]$Topic4, aes(x = !!as.symbol(v), y = proportion)) +
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
plot_logisticn_Date <- plot_logisticn_t + 
  scale_x_continuous(name = "Date", breaks = ticks_date$breaks, labels = ticks_date$labels)
### Combine all plots
gridExtra::grid.arrange(plot_logisticn_Date, plot_logisticn_Struktur_4, 
                        plot_logisticn_Struktur_22, plot_logisticn_Struktur_42, ncol=2, 
                        top = grid::textGrob("Topic 4: Social/Housing", 
                                             gp=grid::gpar(fontsize=16, fontface = "bold")))

## Topic 4: Social/Housing
### Continuous plot with credible intervals
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_logisticn_", v)
  plot_smoothed_ci <- ggplot(preds_logisticn[[v]]$Topic4) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_lower), method = "loess", se = FALSE) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_upper), method = "loess", se = FALSE)
  smoothed_ci <- ggplot_build(plot_smoothed_ci)
  df_smoothed_ci <- data.frame(v = smoothed_ci$data[[1]]$x,
                               ci_lower = smoothed_ci$data[[1]]$y,
                               ci_upper = smoothed_ci$data[[2]]$y)
  assign(plot_nam, plot_smoothed_ci + 
           geom_ribbon(data = df_smoothed_ci, aes(x = v, ymin = ci_lower, ymax = ci_upper), 
                       fill = "grey70") +
           geom_smooth(data = preds_logisticn[[v]]$Topic4, aes(x = !!as.symbol(v), y = proportion),
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
plot_logisticn_Date <- plot_logisticn_t + 
  scale_x_continuous(name = "Date", breaks = ticks_date$breaks, labels = ticks_date$labels)
### Combine all plots
gridExtra::grid.arrange(plot_logisticn_Date, plot_logisticn_Struktur_4, 
                        plot_logisticn_Struktur_22, plot_logisticn_Struktur_42, ncol=2, 
                        top = grid::textGrob("Topic 4: Social/Housing", 
                                             gp=grid::gpar(fontsize=16, fontface = "bold")))

## Topic 4: Social/Housing
### Categorical plot with credible intervals
(plot_logisticn_party_4 <- ggplot(preds_logisticn$Partei$Topic4, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic 4: Social/Housing")+
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
          axis.title.x = element_text(size=16)))
