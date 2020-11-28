######################################################
# Analysing  sentiment scores 
# in directed MANIFESTO categories
#
# Project: Validation of German sentiment dictionaries
#
# To be published in the
# Journal of Information Technology and Politics
#
# Author: christian.rauh@wzb.eu / christian-rauh.eu 
# Date:   13.04.2017
######################################################

# Packages
library(stringr)
library(ggplot2)
library(GGally)


# Set YOUR working directory here (root of replication package)
setwd("M:/user/rauh/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication-Final/")
setwd("C:/Users/CUZ/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication-Final/")


# Load the scored Manifesto texts
#--------------------------------

load(file = "./3_Manifestos/Data/ValidationSampleQuasiSentScored.Rdata")


# Distributions
#--------------

distr <- as.data.frame(table(data$descr, data$cmp_code))
distr <- distr[distr[ ,3] != 0, ]
names(distr) <- c("issue", "cmp_code", "freq")
help  <- aggregate(data$terms, by = list(data$cmp_code), FUN = mean)
names(help) <- c("cmp_code", "length")
distr <- merge(distr, help, by = "cmp_code", all.x = TRUE)

write.table(distr, file = "/3_Manifestos/Data/DistrQuasiSent.csv", sep = ";")

min(data$date)
max(data$date)

length(unique(data$date))
length(unique(data$party))


# Assesing the neutrality bias
#-----------------------------

nrow(data[data$sentiws.norm == 0, ])
(nrow(data[data$sentiws.norm == 0, ]) / nrow(data))  * 100
nrow(data[data$polarity.norm == 0, ])
(nrow(data[data$polarity.norm == 0, ]) / nrow(data))  * 100
nrow(data[data$sentiment.norm == 0, ])
(nrow(data[data$sentiment.norm == 0, ]) / nrow(data) ) * 100

# Exporting extreme disagreements 
# between augmented dictionary and human coder for qualitative analysis
#----------------------------------------------------------------------

false.neg <- data[data$sentiment.norm > .75 & data$direction == "negative", ]
false.pos <- data[data$sentiment.norm < -.75 & data$direction == "positive", ]
failed <- rbind(false.neg, false.pos)
write.table(failed, file = "./3_Manifestos/Data/failedCase.csv", sep = ";")
rm(false.neg, false.pos, failed)


# Analysing mean predictions by issue category
##############################################

# Create a separate 'All' category

data2 <- data
data2$issue <- "All issues"
data3 <- rbind(data, data2)

# Compare discriminatory power of different sentiment dictionaries
# across all observations

abs(mean(data2$sentiws.norm[data2$direction == "positive"]) - mean(data2$sentiws.norm[data2$direction == "negative"]))
abs(mean(data2$polarity.norm[data2$direction == "positive"]) - mean(data2$polarity.norm[data2$direction == "negative"]))
abs(mean(data2$sentiment.norm[data2$direction == "positive"]) - mean(data2$sentiment.norm[data2$direction == "negative"]))


# Some cosmetics

data3$issue <- as.character(data3$issue)
data3$issue[data3$issue == "European Community/Union"] <- "European Union"
data3$issue[data3$issue == "Foreign Special Relationships"] <- "Foreign Spec. Rel."
data3$issue[data3$issue == "National Way of Life"] <- "Nat. way of life"
data3$issue[data3$issue == "Traditional Morality"] <- "Trad. Morality"


# Plot mean comparisons of augmented dictionary across categories 

ggplot(data = data3, aes(x = direction, y = sentiment.norm, colour = direction))+
  geom_hline(yintercept = 0)+
  stat_summary(fun.data = "mean_cl_boot", size = .8)+
  facet_grid(~issue)+
  ylab("Augm. dictionary scores")+xlab("")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_blank())+
  scale_colour_manual(values = c("red", "darkgreen"),
                      name = "Human coded direction")


# Plot distribution of augmented dictionary across categories (Figure B1 in appendix)

ggplot(data = data3, aes(x = direction, y = sentiment.norm))+
  geom_hline(yintercept = 0)+
  geom_jitter(position = position_jitter(width = 1), alpha = 0.2, aes(colour = direction))+
  stat_summary(fun.data = "mean_cl_boot", size = .5, colour = "black")+
  ggtitle("")+
  facet_grid(~issue)+
  ylab("Mean sentiment score (augm. dictionary) / bootstraped 95% c.i\n")+xlab("")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        plot.title = element_text(size = 14, face = "bold", colour = "black", vjust = -1))+
  scale_colour_manual(values = c("red", "darkgreen"),
                      name = "Human coded direction")

ggsave("./3_Manifestos/Plots/FigB1_DistrAugmSentScores.png", width = 36, height = 20, units = "cm")


# Dictionary comparison across Manifesto categories (Figure 3 in main text)
#--------------------------------------------------------------------------

# Turn to long format to later group over dictionary

data4 <- data3[ , c("issue", "direction", "sentiws.norm")]
names(data4) <- c("issue", "direction", "SentScore")
data4$dictionary <- "SentiWS"

temp <- data3[ , c("issue", "direction", "polarity.norm")]
names(temp) <- c("issue", "direction", "SentScore")
temp$dictionary <- "GPC"
data4 <- rbind(data4, temp)

temp <- data3[ , c("issue", "direction", "sentiment.norm")]
names(temp) <- c("issue", "direction", "SentScore")
temp$dictionary <- "Augm. Dictionary"
data4 <- rbind(data4, temp)


# Plot dictionary comparison

ggplot(data = data4, aes(x = direction, y = SentScore, group = dictionary, colour = direction, shape = dictionary))+
  geom_hline(yintercept = 0, linetype="dashed", color = "black")+
  stat_summary(fun.data = "mean_cl_boot", size = .6, position = position_dodge(width = .7))+
  coord_cartesian(ylim = c(-0.11, 0.22))+
  xlab("")+ylab("Mean sentiment score / bootstrapped 95% c.i.\n")+
  ggtitle("")+
  facet_grid(~issue)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        axis.text.x = element_blank(),
        plot.title = element_text(size = 14, face = "bold", colour = "black", vjust = -1))+
  scale_shape_manual(values = c(17, 15, 16),
                     name = "Dictionary: ")+
  scale_colour_manual(values = c("grey60", "black"),
                        name = "Human code: ")

ggsave("./3_Manifestos/Plots/Fig3_DictCompDirecManifesto.tiff", width = 33, height = 15, units = "cm", dpi = 600)



# Sentiment in EU related quais-sentences vs. net-EU-support measure
####################################################################

# Clean slate
rm(list = ls()) 


# Load pooled and scored EU data
load(file = "./3_Manifestos/Data/ValidationSamplePooledScoredRated.Rdata")
data2 <- data2[data2$issue == "European Community/Union", ]

# Get length of coding units
summary(data2$terms)


# Establish the plots for continous and categorical comparision
# of sentiment scores and human-based scale

net.cont <- ggplot(data = data2, aes(x = net.supp.norm, y = sentiment.norm))+
  geom_point(size = 2, color="grey40", shape=1)+
  geom_smooth(method=lm, size = 1, colour = "black")+
  geom_smooth(method=lm, aes(fill=net.supp.norm > 0), se = FALSE, size = 1, linetype="dashed", color = "black")+
  # geom_smooth(method=lm, aes(fill=net.supp.norm > 0 , color=net.supp.norm > 0 ), se = FALSE, size = 1, linetype="dotted")+
  ylab("Sentiment scores\n(Augm. dictionary)\n") + xlab("\nEU net-support (human)\n")+
  theme_bw()+
  theme(legend.position = "none")

net.cat <- ggplot(data = data2, aes(x = net.supp.norm >=0, y = sentiment.norm))+
  stat_summary(fun.data = "mean_cl_boot", size = .8)+
  ylab("Mean sentiment score\n(Augm. dictionary, 95% c.i.)\n") + xlab("\nEU net-support (human) >= 0\n")+
  theme_bw()+
  theme(legend.position = "none")

# Combine and export plots (Figure 4 in main text)
#------------------------------------------------

source("multiplot.R")

tiff("./3_Manifestos/Plots/Fig4_ManifestoEUsupport.tiff",  width = 32, height = 16, unit = "cm", res = 600)
layout <- matrix(c(1, 1, 2), nrow = 1, byrow = TRUE)
multiplot(net.cont, net.cat, nrwo = 1, byrow = TRUE, layout = layout)
dev.off()
