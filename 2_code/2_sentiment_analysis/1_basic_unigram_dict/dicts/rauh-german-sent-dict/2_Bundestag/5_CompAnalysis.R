###############################################################
# Analyse and compare human and automated sentiment codings 
# across 1,500 random sentences from plenary Budnestag speeches 
#
# Project: Validation of German sentiment dictionaries
#
# To be published in the
# Journal of Information Technology and Politics
#
# Author: christian.rauh@wzb.eu / christian-rauh.eu 
# Date:   13.04.2017
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


# Load data
#----------
load("./2_Bundestag/Data/ValidationSampleFinal.Rdata")
data$vote <- factor(data$vote, levels = c("Clearly negative", "Rather negative", "Neutral", "Rather positive", "Clearly positive"))


# Sentence Sample
#################

summary(data$terms)

ggplot(data = data, aes(x = terms))+
  geom_histogram(binwidth = 1)+
  # geom_density()+
  xlab("Sentence length (# of terms)")+
  ylab("Abs. frequency")+
  theme_bw()

ggsave("./2_Bundestag/Plots/FigA1_SentenceLength.png", width = 12, height = 12, units = "cm")


# Distributions
###############

# Histogram of human majority vote
# png("./2_Bundestag/Plots/FreqHumanMajorityVotesSentiment.png",  width = 300, height = 450)
# plot(data$vote)
# dev.off()

# Coders
plot(factor(data$coder1), main = "Coder 1", ylim=c(0, 800))
plot(factor(data$coder2), main = "Coder 2", ylim=c(0, 800))
plot(factor(data$coder3), main = "Coder 3", ylim=c(0, 800))

# Sentiment score
summary(data$sentiment.norm)
hist(data$sentiment.norm, breaks = seq(-1, 1, length.out = 6), ylim=c(0, 1300), main = "Normalized sentiment score")


# Plot combined distributions of sentiment scores
# Figure 1 in main text
#-------------------------------------------------

# str(data) # To identify relevant columns
# 14, 9, 7

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
                     columns = c(7, 9, 14), 
                     columnLabels = c("SentiWS", "GPC", "Augm. Dict."),
                     
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

# png("./2_Bundestag/Plots/Fig1_SentiDictsBundestagSentences.png",  width = 700, height = 700)
tiff("./2_Bundestag/Plots/Fig1_SentiDictsBundestagSentences_600dpi.tiff", width = 16, height = 16, units = 'cm', res = 600)
print(pair.plot + theme_bw())
dev.off()




# Plot the scores vs human coders
# over the different dictionaries
# Figure 2 in main text
#################################

# Long data format

data2 <- data[ , c("vote", "polarity.norm", "sentiws.norm", "sentiment.norm")]
data2 <- melt(data2, id = "vote")

data2$Dictionary <- as.character(data2$variable)
data2$Dictionary[data2$Dictionary == "polarity.norm"] <- "GPC"
data2$Dictionary[data2$Dictionary == "sentiws.norm"] <- "SentiWS"
data2$Dictionary[data2$Dictionary == "sentiment.norm"] <- "Augm. Dict."

data2$Dictionary <- factor(data2$Dictionary, levels = c("Augm. Dict.", "GPC", "SentiWS"))
data2$Dictionary <- factor(data2$Dictionary, levels=rev(levels(data2$Dictionary)))

ggplot(data = data2, aes(x = vote, y = value, shape = Dictionary))+
  geom_hline(yintercept = 0, linetype="dashed", color = "black")+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, position=position_dodge(width=0.3))+
  stat_summary(fun.y = mean, geom="line", aes(group=Dictionary), linetype="dotted", position = position_dodge(width = .3))+
  # scale_colour_manual(values = c("darkgreen",  "darkblue", "darkred"))+
  scale_shape_manual(values = c(15, 17, 19))+
  ylab("Normalized sentiment scores\n(Means + bootstrapped 95% c.i)\n")+
  xlab("\nHuman coders (majority vote)")+
  theme_bw()+
  theme(plot.title = element_text(size = 18, face = "bold", colour = "black", vjust = -1),
        axis.text=element_text(size=12),
        legend.position = "bottom")

ggsave("./2_Bundestag/Plots/Fig2_ScoresVSmajorityVote.tiff", width = 25, height = 16, units = "cm", dpi = 600)


# Plot distribution of aug. dictionary scores
# over human-coded categories
# Figure A3 in Appendix
##############################################

ggplot(data = data, aes(x = vote, y = sentiment.norm))+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  geom_jitter(position = position_jitter(width = .3), alpha = 0.2)+
  stat_summary(fun.y = mean, geom="line", aes(group=1), colour = "red", linetype="dashed")+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, colour = "red")+
  ylab("Normalized Sentiment Score\n(Augm. dictionary)\n")+
  xlab("\nHuman coders (majority vote)")+
  theme_bw()+
  theme(plot.title = element_text(size = 18, face = "bold", colour = "black", vjust = -1),
        axis.text=element_text(size=12))

ggsave("./2_Bundestag/Plots/FigA3_DistrScoresVSmajorityVote.png", width = 25, height = 16, units = "cm")


# Export 'wrong' classifications for qualitative analysis
#########################################################

false.positives <- data[data$vote == "Clearly negative" & data$sentiment.norm > 0, ]
write.table(false.positives, file = "./2_Bundestag/Data/FalsePositives.csv", sep = ";", row.names = FALSE)

false.negatives <- data[data$vote == "Clearly positive" & data$sentiment.norm < 0, ]
write.table(false.negatives, file = "./2_Bundestag/Data/FalseNegatives.csv", sep = ";", row.names = FALSE)

false.neutrals <- data[data$vote == "Clearly negative" & data$sentiment.norm == 0, ]
write.table(false.neutrals, file = "./2_Bundestag/Data/FalseNeutrals.csv", sep = ";", row.names = FALSE)



# Decrease resolution of sentiment scores
# to the three point scale humans faced
#########################################

# Treshold of one sd

data$sentiment.crude <- ifelse(data$sentiment.norm > (mean(data$sentiment.norm) + sd(data$sentiment.norm)), 1,
                               ifelse(data$sentiment.norm < (mean(data$sentiment.norm) - sd(data$sentiment.norm)), -1 , 0))

data$sentiws.crude <- ifelse(data$sentiws.norm > (mean(data$sentiws.norm) + sd(data$sentiws.norm)) , 1,
                             ifelse(data$sentiws.norm < (mean(data$sentiws.norm) - sd(data$sentiws.norm)) , -1 , 0))

data$polarity.crude <- ifelse(data$polarity.norm > (mean(data$polarity.norm) + sd(data$polarity.norm)), 1,
                              ifelse(data$polarity.norm < (mean(data$polarity.norm) - sd(data$polarity.norm)), -1 , 0))



# Human codings to int scale
############################

data$coder11 <- data$coder1
data$coder22 <- data$coder2
data$coder33 <- data$coder3

data$coder11[data$coder11 == "Neutral"] <- 0
data$coder11[data$coder11 == "Positiv"] <- 1
data$coder11[data$coder11 == "Negativ"] <- -1

data$coder22[data$coder22 == "Neutral"] <- 0
data$coder22[data$coder22 == "Positiv"] <- 1
data$coder22[data$coder22 == "Negativ"] <- -1

data$coder33[data$coder33 == "Neutral"] <- 0
data$coder33[data$coder33 == "Positiv"] <- 1
data$coder33[data$coder33 == "Negativ"] <- -1


# Create a correlation matrix
# Figure A3 in appendix
#----------------------------

data3 <- as.data.frame(data$sentiment.crude)
names(data3) <- "Augm. Dict."

data3$SentiWS <- data$sentiws.crude
data3$GPC <- data$polarity.crude
data3$Coder1 <- as.numeric(data$coder11)
data3$Coder2 <- as.numeric(data$coder22)
data3$Coder3 <- as.numeric(data$coder33)

M <- cor(data3, method = "spearman")
corrplot(M, method="circle")

png("./2_Bundestag/Plots/Fig2_SpearmanCorrelationsCoders.png",  width = 500, height = 500)
corrplot.mixed(M)
dev.off()


# Inter-rater agreement
# Table A1 in appendix
#----------------------

data4 <- data3

for (i in 1:ncol(data4)) {
  data4[ ,i] <- gsub("-1", "Negative", data4[ ,i])
  data4[ ,i] <- gsub("0", "Neutral", data4[ ,i])
  data4[ ,i] <- gsub("1", "Positive", data4[ ,i])
}

# Calculate percentage agreements 
# on the basis of the IRR package

humans <- data4[ , 4:6]
dictionaries <- data4[ ,1:3]

agree(humans)
agree(dictionaries)
agree(data4)

humans.m <- t(as.matrix(humans))
kripp.alpha(humans.m, method = "ordinal")

dictionaries.m <- t(as.matrix(dictionaries))
kripp.alpha(dictionaries.m, method = "ordinal")

all.m <- t(as.matrix(data4))
kripp.alpha(all.m, method = "ordinal")


# Count neutral observations per 'coder'
# Table 1 in main text
#-----------------------------------------

for (i in 1:ncol(data4)){
  print(names(data4)[i])
  print(sum(data4[ ,i] == "Neutral"))
  print((sum(data4[ ,i] == "Neutral")/1500)*100)
}

