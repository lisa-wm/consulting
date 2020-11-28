################################################################
# Extract random sample from 'Causa Wulff' media coverage for 
# human coding
#
# Author: CHRISTIAN RAUH - 15.02.2017
#################################################################


library(stringr)
library(tidyverse)
library(ggplot2)
library(cowplot)
# library(GGally)
# library(reshape2)
# library(corrplot)
# library(plyr)
# library(irr)


setwd("C:/Users/CUZ/WZB_CR/Datensaetze/SentiWS_Validation/CausaWulff") # CUZ Dell
setwd("M:/user/rauh/WZB_CR/Datensaetze/SentiWS_Validation/CausaWulff") # WZB CR

# Load the data
###############

load("WulffCorpusScored.Rdata")

# Reduce to investigation period Octorber 2010 to April 2012 
data <- data[data$pub_date > as.Date("2011-10-01") & data$pub_date < as.Date("2012-05-01"), ]


# Draw random sample from data frame
####################################

# Set the seed for consistent replication
set.seed(20170215) 

# Draw an n = 100 sample of newspaper articles
sample.data <- sample_n(data, size = 100, replace = FALSE)


# Compare the sample properties to original data frame
######################################################

# Sentiment

sent.distr <- ggplot(data = data, aes(x = sentiment.norm))+
  geom_histogram(binwidth = 0.001)+
  ggtitle("Sentiment: Original data")+
  xlab("Augmented Sentiment Scores")+
  coord_cartesian(xlim = c(-0.2, 0.2))+ 
  theme_bw()

sample.distr <- ggplot(data = sample.data, aes(x = sentiment.norm))+
  geom_histogram(binwidth = 0.001)+
  coord_cartesian(xlim = c(-0.2, 0.2))+ 
  ggtitle("Sentiment: Sample data")+
  xlab("Augmented Sentiment Scores")+
  theme_bw()

sentiment.comparision <- plot_grid(sent.distr, sample.distr)

# Article length

sent.distr <- ggplot(data = data, aes(x = terms))+
  geom_histogram(binwidth = 1)+
  ggtitle("Article length: Original data")+
  xlab("Article length")+
  coord_cartesian(xlim = c(0, 5000))+ 
  theme_bw()

sample.distr <- ggplot(data = sample.data, aes(x = terms))+
  geom_histogram(binwidth = 1)+
  coord_cartesian(xlim = c(0, 5000))+ 
  ggtitle("Article length: Sample data")+
  xlab("Article length")+
  theme_bw()

length.comparison <- plot_grid(sent.distr, sample.distr)

# Store article sample
######################

# Unique id for merging later
sample.data$id <- 1:nrow(sample.data)
sample.data$id <- paste("ID-", sample.data$id, sep = "")

# Export the sample - basis for merge
save(sample.data, file = "./HumanSample/HumanSample.Rdata")


# Extract samples for human coders
##################################

# Tasks: 
# Randomize order for each coder
# Create txt files for each coder
# Create coding sheet for each coder with id and filename


# Coder 1 (Johannes?)
#--------------------

# Copy
coder1 <- sample.data

# Random order
set.seed(111)
coder1 <- sample_n(coder1, size = 100, replace = FALSE)

# Filenames
coder1$file <- 1:nrow(coder1)
coder1$file <- paste("FILE-", coder1$file, sep = "")

coder1$file2 <- paste("./HumanSample/Coder1/", coder1$file, ".txt", sep = "")

# Export textfiles
for (i in 1:nrow(coder1)){
  write.table(coder1$raw_text[i], file = coder1$file2[i], row.names = FALSE, col.names = FALSE)
}

# Export basis for coding sheet
write.table(coder1[ , c("pub_date", "newspaper", "id", "file")], file = "./HumanSample/Coder1/SheetBasis.csv", sep = ";", row.names = FALSE)


# Coder 2 (Max ?)
#--------------------

# Copy
coder2 <- sample.data

# Random order
set.seed(222)
coder2 <- sample_n(coder2, size = 100, replace = FALSE)

# Filenames
coder2$file <- 1:nrow(coder2)
coder2$file <- paste("FILE-", coder2$file, sep = "")

coder2$file2 <- paste("./HumanSample/Coder2/", coder2$file, ".txt", sep = "")

# Export textfiles
for (i in 1:nrow(coder2)){
  write.table(coder2$raw_text[i], file = coder2$file2[i], row.names = FALSE, col.names = FALSE)
}

# Export basis for coding sheet
write.table(coder2[ , c("pub_date", "newspaper", "id", "file")], file = "./HumanSample/Coder2/SheetBasis.csv", sep = ";", row.names = FALSE)

