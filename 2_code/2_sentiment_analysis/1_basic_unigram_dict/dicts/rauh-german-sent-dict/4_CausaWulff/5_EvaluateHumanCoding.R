#####################################################
# Compare human and machine coding 
# for the 'Causa Wulff' sample of newspaper articles
#
# Project: Validation of German sentiment dictionaries
#
# To be published in the
# Journal of Information Technology and Politics
#
# Author: CHRISTIAN RAUH - 15.02.2017
#####################################################


# Packages

library(stringr)
library(tidyverse)
library(ggplot2)
library(reshape2)

# Working directory
setwd("M:/user/rauh/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication-Final/")
setwd("C:/Users/CUZ/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication-Final/")



# Load the machine-coded data
##############################

load("./4_CausaWulff/HumanSample/HumanSample.Rdata")


# Load coder 1 data
###################

coder1 <- read.csv2("./4_CausaWulff/HumanSample/Coder1/Coder1_NoNeut.csv", header = TRUE, stringsAsFactors = FALSE)
table(coder1$Code)

coder1$Coder1 <- factor(coder1$Code, levels = c("Sehr negativ", "Eher negativ", "Eher positiv", "Sehr positiv"))
table(coder1$Coder1)

coder1 <- coder1[ ,c("id", "Coder1")]


# Load coder 2 data
###################

coder2 <- read.csv2("./4_CausaWulff/HumanSample/Coder2/Coder2_NoNeut.csv", header = TRUE, stringsAsFactors = FALSE)
table(coder2$Code)

coder2$Coder2 <- factor(coder2$Code, levels = c("Sehr negativ", "Eher negativ", "Eher positiv", "Sehr positiv"))
table(coder2$Coder2)

coder2 <- coder2[ ,c("id", "Coder2")]


# Merge the data
################

data <- merge(sample.data, coder1, by = "id")
data <- merge(data, coder2, by = "id")

rm(sample.data, coder1, coder2)


# Compare coders' choices
#########################

# Percentage agreement
(sum(data$Coder1 == data$Coder2)* 100)/nrow(data)

data$agreement <- ifelse(data$Coder1 == data$Coder2, TRUE, FALSE)

# Plot disagreement
ggplot(data = data, aes(x=Coder1, y=Coder2))+
  # geom_abline(intercept = 0, slope = 1)+
  geom_jitter(width = .1, height = .1, alpha = .5, size = 2, aes(colour = agreement))+
  scale_color_manual(values = c("red", "darkgreen"))+
  theme(legend.position = "none")


# Compare coders to automated sentiment scores
# With aggregated answer categories
# Figure 6 in main text
##############################################

# Allow only negative or positive

data$Coder11 <- 0
data$Coder11[as.numeric(data$Coder1) <= 2] <- -1
data$Coder11[as.numeric(data$Coder1) > 2] <- 1

data$Coder22 <- 0
data$Coder22[as.numeric(data$Coder2) <= 2] <- -1
data$Coder22[as.numeric(data$Coder2) > 2] <- 1

# Percentage agreement
(sum(data$Coder11 == data$Coder22)* 100)/nrow(data)

# Establish the majority vote
data$Coders <- data$Coder11 + data$Coder22
table(data$Coders)

data$Coders[data$Coders == -2 ] <- "clearly\nnegative"
data$Coders[data$Coders == 2 ] <- "clearly\npositive"
data$Coders[data$Coders == 0 ] <- "neutral/\nunclear"

data$Coders <- factor(data$Coders, levels = c("clearly\nnegative", "neutral/\nunclear", "clearly\npositive"))

# Reshape data to long format
long1 <- data[ , c("id", "Coders", "polarity.norm", "sentiws.norm", "sentiment.norm")]
long2 <- melt(long1, id.vars = c("id", "Coders"))

long2$Dictionary <- as.character(long2$variable)
long2$Dictionary[long2$Dictionary == "polarity.norm"] <- "GPC"
long2$Dictionary[long2$Dictionary == "sentiws.norm"] <- "Senti WS"
long2$Dictionary[long2$Dictionary == "sentiment.norm"] <- "Augm. Dictionary"

long2$Dictionary <- factor(long2$Dictionary, levels = c("Augm. Dictionary", "GPC", "Senti WS"))
long2$Dictionary <- factor(long2$Dictionary, levels=rev(levels(long2$Dictionary)))

# And plot
ggplot(data = long2, aes(x = Coders, y = value, shape = Dictionary))+
  geom_hline(yintercept = 0, linetype="dashed", color = "black")+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, position=position_dodge(width=0.2))+
  stat_summary(fun.y = mean, geom="line", aes(group=Dictionary), linetype="dashed", position = position_dodge(width = .1))+
  # scale_colour_manual(values = c("darkgreen",  "darkblue", "darkred"))+
  scale_shape_manual(values = c(15, 17, 19))+
  ylab("Normalized sentiment scores\n(means + bootstrapped 95% c.i)")+
  xlab("\nHuman coders")+
  theme_bw()+
  theme(plot.title = element_text(size = 18, face = "bold", colour = "black", vjust = -1),
        axis.text=element_text(size=12),
        legend.position = "right")

ggsave("./4_CausaWulff/Plots/Fig6_HumanCodersWulff.png", width = 20, height = 12, units = "cm", dpi = 600)


# Compare mean spreads
######################

# GPC
mean(long2$value[as.numeric(long2$Coders) == 3 & long2$Dictionary == "GPC"]) - mean(long2$value[as.numeric(long2$Coders) == 1 & long2$Dictionary == "GPC"])

# Augm. dictionary
mean(long2$value[as.numeric(long2$Coders) == 3 & long2$Dictionary == "Augm. Dictionary"]) - mean(long2$value[as.numeric(long2$Coders) == 1 & long2$Dictionary == "Augm. Dictionary"])

