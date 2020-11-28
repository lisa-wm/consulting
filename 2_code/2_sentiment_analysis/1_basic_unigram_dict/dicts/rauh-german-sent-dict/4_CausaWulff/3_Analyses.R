################################################################
# Analyse and compare automated sentiment scoring in the sample 
# of 'Causa Wulff' media coverage
#
# Project: Validation of sentiment dictionaries
#
# Author: christian.rauh@wzb.eu / christian-rauh.eu 
# Date:   12.04.2017
###############################################################

# Packages
library(stringr)
library(ggplot2)
library(GGally)
library(reshape2)
library(corrplot)
library(plyr)
library(irr)


# Set YOUR working directory here (root of replication package)
setwd("M:/user/rauh/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication-Final/")
setwd("C:/Users/CUZ/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication-Final/")

# Load the data
###############

load("./4_CausaWulff/WulffCorpusScored.Rdata")

# Reduce to investigation period Octorber 2010 to April 2012 
data <- data[data$pub_date > as.Date("2011-10-01") & data$pub_date < as.Date("2012-05-01"), ]


# Descriptives of the text sample
#################################

# Number of terms per article
data$article_length <- str_count(data$raw_text," ") + 1

# Distribution
summary(data$article_length)

ggplot(data = data, aes(x = article_length))+
  geom_histogram(binwidth = 1)+
  # geom_density()+
  xlab("Article length (# of terms)")+
  ylab("Abs. frequency")+
  ylim(c(0,13))+
  theme_bw()

ggsave("./4_CausaWulff/Plots/FigC2_WulffArticleLength.png", width = 11, height = 11, units = "cm")


# Distribution of articles of over time
#######################################

# Get week of article publication date
data$year <- sub("-.*", "", as.character(data$pub_date), fixed = FALSE)
data$week <- format(data$pub_date, "%U")
data$week <- paste(data$year, "-", data$week, sep ="")

# Identify key events of Causa Wulf in weekly data

weeks <- as.data.frame(unique(data$week))

weeks$event <- FALSE
weeks$event <- ifelse(weeks[,1] == unique(data$week[data$pub_date == as.Date("2011-12-13")]), # Report on credit affair,
                      TRUE, weeks$event)
weeks$event <- ifelse(weeks[,1] == unique(data$week[data$pub_date == as.Date("2012-01-01")]), # Report on Wulff's call to Diekmann
                      TRUE, weeks$event)
weeks$event <- ifelse(weeks[,1] == unique(data$week[data$pub_date == as.Date("2012-02-17")]), # Resignation
                      TRUE, weeks$event)

sum(weeks$event)
which(weeks$event) # Index value

# Plot

ggplot(data = data, aes(x = week))+
  geom_bar(stat="count", colour = "grey", fill = "grey")+
  geom_vline(xintercept = c(11,14,20), colour = "black", linetype = "dashed", size = .5)+
  geom_text(aes(x = 11, label="\nLoan affair", y=599), colour="black", angle=90, size=3, hjust = 1) +
  geom_text(aes(x = 14, label="\nCall affair", y=599), colour="black", angle=90, size=3, hjust = 1) +
  geom_text(aes(x = 20, label="\nResignation", y=599), colour="black", angle=90, size=3, hjust = 1) +
  xlab("\nWeek")+
  ylab("Number of articles\n")+
  theme_bw()+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))

ggsave("./4_CausaWulff/Plots/FigC1_WulffArticlesOverTime.png", width = 16, height = 10, units = "cm")



# Frequency of articles scored as neutral 
#########################################

# Senti WS
sum(data$sentiws.norm == 0)
(sum(data$sentiws.norm == 0)/nrow(data))*100

# GPC
sum(data$polarity.norm == 0)
(sum(data$polarity.norm == 0)/nrow(data))*100

# Augmented dictionary
sum(data$sentiment.norm == 0)
(sum(data$sentiment.norm == 0)/nrow(data))*100


# Uni- and bivariate distributions of dictionary scores
# Figure 5 in main text
#######################################################

# str(data) # Identify relevant columns with normalized sentiment scores
# 10, 12, 17

# Custom plot function

plot.func <- function(data, mapping, ...){ # Function to build custom plot for lower diag in GGpairs
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(color="grey50", shape=1) +
    geom_smooth(method=lm, color="grey25", se = FALSE, ...)+
    geom_smooth(method=loess, color="black", linetype = "dotted", se = FALSE, ...) +
    theme_bw()
  p
}

# Paired plot

pair.plot <- ggpairs(data = data, 
                     
                     # Columns
                     columns = c(10, 12, 17), 
                     columnLabels = c("SentiWS", "GPC", "Augm. Dictionary"),
                     
                     # Above diagonal
                     upper = list(continuous = wrap("cor", size = 12)),
                     
                     # Below diagonal
                     # lower = list(continuous = c("points", "smooth_loess")),
                     # lower = list(continuous = "points"),
                     lower = list(continuous = plot.func),
                     
                     # Diagonal
                     diag = list(combo = "facetdensity"),
                     
                     # Aesthetics
                     # title = "Variable distributions & bi-variate relations"
)


# Save

tiff("./4_CausaWulff/Plots/Fig5_SentiDicsWulffCompared.tiff", width = 16, height = 16, units = 'cm', res = 600)
print(pair.plot + theme_bw())
dev.off()


# Tracking sentiment throughout the course of the 'Causa Wulff'
##############################################################

# A variable capturing the stage of the scandal
# Breakpoints: Loan affair, call affair, resignation

data$dscandal <- 0
data$dscandal[data$pub_date >= "2011-12-13" & data$pub_date < "2012-01-01"] <- 1
data$dscandal[data$pub_date >= "2012-01-01" & data$pub_date < "2012-02-17"] <- 2
data$dscandal[data$pub_date >= "2012-02-17" ] <- 3

# Augmented Dictionary: Linear trends over scandal stages

ggplot(data = data, aes(x = pub_date))+
  scale_x_date()+
  geom_vline(xintercept = as.numeric(as.Date("2011-12-13")), colour = "black", size = .5)+
  geom_vline(xintercept = as.numeric(c(as.Date("2012-01-01"), as.Date("2012-02-17"))), colour = "black", linetype = "dashed", size = .5)+
  geom_text(aes(x = as.Date("2011-12-13"), label="\nLoan affair", y=0.0345), colour="black", angle=90, size=3, hjust = 1) +
  geom_text(aes(x = as.Date("2012-01-01"), label="\nCall affair", y=0.0345), colour="black", angle=90, size=3, hjust = 1) +
  geom_text(aes(x = as.Date("2012-02-17"), label="\nResignation", y=0.0345), colour="black", angle=90, size=3, hjust = 1) +
  stat_smooth(aes(y = sentiment.norm, group = dscandal), method = "lm", colour = "red", size = .5)+
  xlab("Days")+
  ylab("Linear trends\nof daily sentiment scores (augm. dictionary)\n")+
  theme_bw()+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))

# ggsave("./4_CausaWulff/Plots/AugmDict_LinearTrends.png", width = 20, height = 12, units = "cm")

# Augmented Dictionary: Smoothed trend over scandal stages
# Figure 7 in main text

ggplot(data = data, aes(x = pub_date))+
  scale_x_date(date_breaks = 'month')+
  geom_vline(xintercept = as.numeric(as.Date("2011-12-13")), colour = "black", size = .5)+
  geom_vline(xintercept = as.numeric(c(as.Date("2012-01-01"), as.Date("2012-02-17"))), colour = "black", linetype = "dashed", size = .5)+
  geom_text(aes(x = as.Date("2011-12-13"), label="\nLoan affair", y=0.0345), colour="black", angle=90, size=3, hjust = 1) +
  geom_text(aes(x = as.Date("2012-01-01"), label="\nCall affair", y=0.0345), colour="black", angle=90, size=3, hjust = 1) +
  geom_text(aes(x = as.Date("2012-02-17"), label="\nResignation", y=0.0345), colour="black", angle=90, size=3, hjust = 1) +
  stat_smooth(aes(y = sentiment.norm), method = "loess", span = .11, colour = "black")+
  xlab("Days")+
  ylab("LOESS smoother (span = .1)\nof daily sentiment scores (augm. dictionary)\n")+
  theme_bw()+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))

ggsave("./4_CausaWulff/Plots/Fig7_AugmDict_SmoothedTrend.tiff", width = 20, height = 14, units = "cm", dpi = 600)


# Dictionary comparison: Linear trends over scandal stages
# Figure C3 in Appendix

ggplot(data = data, aes(x = pub_date))+
  scale_x_date()+
  geom_vline(xintercept = as.numeric(as.Date("2011-12-13")), colour = "black", size = 1)+
  geom_vline(xintercept = as.numeric(c(as.Date("2012-01-01"), as.Date("2012-02-17"))), colour = "black", linetype = "dashed", size = 1)+
  geom_text(aes(x = as.Date("2011-12-13"), label="\nLoan affair", y=0.1), colour="black", angle=90, size=3) +
  geom_text(aes(x = as.Date("2012-01-01"), label="\nCall affair", y=0.1), colour="black", angle=90, size=3) +
  geom_text(aes(x = as.Date("2012-02-17"), label="\nResignation", y=0.1), colour="black", angle=90, size=3) +
  stat_smooth(aes(y = sentiment.norm, group = dscandal), method = "lm", colour = "red", size = .5)+
  stat_smooth(aes(y = polarity.norm, group = dscandal), method = "lm", colour = "blue", size = .5)+
  stat_smooth(aes(y = sentiws.norm, group = dscandal), method = "lm", colour = "darkgreen", size = .5)+
  geom_text(aes(x = as.Date("2011-10-03"), label="Senti WS", y=.096), colour="darkgreen", angle=0, size=2, hjust = 0) +
  geom_text(aes(x = as.Date("2011-10-03"), label="GPC", y=-0.001), colour="blue", angle=0, size=2, hjust = 0) +
  geom_text(aes(x = as.Date("2011-10-03"), label="Augm. Dictionary", y=.024), colour="red", angle=0,size=2, hjust = 0) +
  xlab("Days")+
  ylab("Linear trends of daily sentiment scores\nin Wulff-related media coverage\n")+
  theme_bw()+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))

ggsave("./4_CausaWulff/Plots/FigC3_DictComp_LinearTrends.png", width = 20, height = 12, units = "cm")


