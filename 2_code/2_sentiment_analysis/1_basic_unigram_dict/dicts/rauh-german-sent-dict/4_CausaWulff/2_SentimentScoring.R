##############################################################
# Scoring German newspaper articles on 'politicans'Causa Wulff 
# with different sentiment dictionaries
#
# Project: Validation of sentiment dictionaries
#
# Author: christian.rauh@wzb.eu / christian-rauh.eu 
# Date:   28.03.2017
###############################################################

# Packages
library(stringr) # 1.0.0
library(ggplot2) # 
library(sm) # 


# Set YOUR working directory here (root of replication package)
setwd("M:/user/rauh/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication/")
setwd("C:/Users/CUZ/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication/")


# Load and prepare the text sample
####################################

load("./4_CausaWulff/WulffCorpus.Rdata") 
data <- corpus
rm(corpus)


# Prepare the source texts
#-------------------------

data$sent.text <- tolower(data$raw_text) # Everything to lower case
data$sent.text <- str_trim(data$sent.text, side = "both")
data$sent.text <- paste(" ", data$sent.text, " ", sep="") # Add white space in the beginning and end
data$sent.text <- gsub("[[:punct:]]", "", data$sent.text, fixed = FALSE) # Remove punctuation
data$sent.text <- gsub("[0-9]", "", data$sent.text, fixed = FALSE) # Remove numbers

data$sent.text <- gsub("  ", " ", data$sent.text, fixed = TRUE) # Remove doubled whitespaces
data$sent.text <- gsub("  ", " ", data$sent.text, fixed = TRUE) # Recursive, to be sure
data$sent.text <- gsub("  ", " ", data$sent.text, fixed = TRUE) # Recursive, to be sure


# Count length of text
#---------------------

data$terms <- str_count(data$sent.text, " ") - 1  # Number of terms based on number of spaces (preceding whitespace subtracted)


# Removing stopwords from length count
# Using the German StopWord list supplied with the Snowball Stemmer

stopwords <- as.data.frame(readLines("./1_Dictionaries/SnowballStopwordsGerman.txt"))
names(stopwords) <- "term"
stopwords$term <- sub("\\|.*", "", stopwords$term, fixed = FALSE) # Remove comments from file
stopwords <- stopwords[grepl("[a-z]", stopwords$term), ] # keep only lines with terms
stopwords <- str_trim(stopwords) # remove superfluous whitespaces
stopwords <- paste(stopwords, collapse = " | ") # Regular 'or' expression
stopwords <- paste(" ", stopwords, " ", sep = "") # Add whitespace left and right
stopwords <- paste(stopwords, "| dass ", sep = "") # neue Rechtschreibung ;)

data$stopwords <- str_count(data$sent.text, stopwords) # Count stopwords in text
data$terms.raw <- data$terms # Copy raw term count
data$terms <- data$terms.raw - data$stopwords # Correct term count by subtracting stopwords
data$terms[data$terms == 0] <- 1 # To avoid dividing by zero, doesn't change sentiment because no weighted terms can be in there



# Sentiment dictionaries and coding
###################################
###################################

# Senti WS
##########

# Load and prepare the dictionary
#--------------------------------

# Positive terms

posterms <- read.delim(file = "./1_Dictionaries/SentiWS_v1.8c_Positive.txt", sep = "\t", header = FALSE)
names(posterms) <- c("term", "sentweight", "inflections")

posterms$term <- gsub("\\|.*$", "", posterms$term, fixed = FALSE) # removes POS tags
posterms$term <- str_trim(posterms$term, side = "both")
posterms$term <- paste(" ", posterms$term, " ", sep = "")

posterms$inflections <- str_trim(posterms$inflections, side = "both")
posterms$inflections <- paste(" ", posterms$inflections, " ", sep = "")
posterms$inflections <- gsub(",", " | ", posterms$inflections, fixed = TRUE)

Encoding(posterms$term) <- "UTF-8"
Encoding(posterms$inflections) <- "UTF-8"

posterms$term <- tolower(posterms$term)
posterms$inflections <- tolower(posterms$inflections)

posterms <- posterms[order(-posterms$sentweight), ]


# Negative terms

negterms <- read.delim(file = "./1_Dictionaries/SentiWS_v1.8c_Negative.txt", sep = "\t", header = FALSE)
names(negterms) <- c("term", "sentweight", "inflections")

negterms$term <- gsub("\\|.*$", "", negterms$term, fixed = FALSE) # removes POS tags
negterms$term <- str_trim(negterms$term, side = "both")
negterms$term <- paste(" ", negterms$term, " ", sep = "")

negterms$inflections <- str_trim(negterms$inflections, side = "both")
negterms$inflections <- paste(" ", negterms$inflections, " ", sep = "")

negterms$inflections <- gsub(",", " | ", negterms$inflections, fixed = TRUE)

Encoding(negterms$term) <- "UTF-8"
Encoding(negterms$inflections) <- "UTF-8"

negterms$term <- tolower(negterms$term)
negterms$inflections <- tolower(negterms$inflections)

negterms <- negterms[order(negterms$sentweight), ]


# Replacing inflections / synonyms in the source text
#----------------------------------------------------

data$sent.text2  <- data$sent.text # Copy of the sampled texts

for (i in 1:nrow(posterms)) {
  data$sent.text2 <- gsub(posterms$inflections[i], posterms$term[i], data$sent.text2, fixed = FALSE)
}

for (i in 1:nrow(negterms)) {
  data$sent.text2 <- gsub(negterms$inflections[i], negterms$term[i], data$sent.text2, fixed = FALSE)
}


# Ensure only unique observations (same spelling, different pos)
#---------------------------------------------------------------
posterms <- as.data.frame(unique(posterms$term))
names(posterms) <- "term"
posterms$term <- as.character(posterms$term)
negterms <- as.data.frame(unique(negterms$term))
names(negterms) <- "term"
negterms$term <- as.character(negterms$term)



# Scoring
#--------
# Negation and weights ignored


data$sentiws <- 0

# Positive terms, increase count by number of occurences

for (i in 1:nrow(posterms)) {
  occur <- str_count(data$sent.text2, posterms$term[i])
  data$sentiws <- data$sentiws + occur                     
}

# Negative terms, decrease count by number of occurences

for (i in 1:nrow(negterms)) {
  occur <- str_count(data$sent.text2, negterms$term[i])
  data$sentiws <- data$sentiws - occur                      
}

# Normalize along length of scored text
data$sentiws.norm <- data$sentiws / data$terms

# Remove SENTI WS dictionary from workspace
rm("posterms" , "negterms")

# Remove pre-processed text
data$sent.text2 <- NULL

# Look at the distribution
hist(data$sentiws.norm, breaks =35)



# German Polarity Clues
#######################


# Load and prepare the dictionary
#--------------------------------
# Source: http://www.ulliwaltinger.de/sentiment/ (28.07.2015)
# Note that inflections are separate oberservations - no pre-processing of the text necessary

posterms <- read.table(file = "./1_Dictionaries/GermanPolarityClues-Positive-21042012.tsv", sep = "\t")
negterms <- read.table(file = "./1_Dictionaries/GermanPolarityClues-Negative-21042012.tsv", sep = "\t")

names(posterms) <- c("term", "lemma", "POS", "sentiment", "weights", "noidea")
names(negterms) <- c("term", "lemma", "POS", "sentiment", "weights", "noidea")

# Remove superfluous whitespaces
posterms$term <- str_trim(posterms$term, side = "both")
negterms$term <- str_trim(negterms$term, side = "both")

# Add exactly one left and right 
posterms$term <- paste(" ", posterms$term, " ", sep = "")
negterms$term <- paste(" ", negterms$term, " ", sep = "")

# Ensuring correct encoding
Encoding(posterms$term) <- "UTF-8"
Encoding(negterms$term) <- "UTF-8"

# To lower
posterms$term <- tolower(posterms$term)
negterms$term <- tolower(negterms$term)


# Ensure only unique observations (same spelling, different pos)
#---------------------------------------------------------------
posterms <- as.data.frame(unique(posterms$term))
names(posterms) <- "term"
posterms$term <- as.character(posterms$term)
negterms <- as.data.frame(unique(negterms$term))
names(negterms) <- "term"
negterms$term <- as.character(negterms$term)


# Scoring
#--------

data$polarity <- 0

for (i in 1:nrow(posterms)) {
  occur <- str_count(data$sent.text, posterms$term[i])
  data$polarity <- data$polarity + occur                  
}

for (i in 1:nrow(negterms)) {
  occur <- str_count(data$sent.text, negterms$term[i])
  data$polarity <- data$polarity - occur                      
}


# Normalize along length of scored text
data$polarity.norm <- data$polarity / data$terms

# Remove Polarity Clues dictionary from workspace
rm("posterms" , "negterms")

# Look at the distribution
hist(data$polarity.norm, breaks =35)


# The combined dictionary (Rauh)
###############################

load("./1_Dictionaries/Rauh_SentDictionaryGerman.Rdata") 
load("./1_Dictionaries/Rauh_SentDictionaryGerman_Negation.Rdata") 

# Marking directly negated sentiment features in the source text
#---------------------------------------------------------------

data$sent.text2 <- data$sent.text # Copy of the sampled sent.texts

# Loop over negation dictionary, and replace instances in text
for (i in 1:nrow(neg.sent.dictionary)){
  data$sent.text2 <- gsub(neg.sent.dictionary$pattern[i], neg.sent.dictionary$replacement[i], data$sent.text2, fixed = FALSE)
}

# How many instances were affected?
nrow(data[grepl("NOT_", data$sent.text2), ]) # Abs
nrow(data[grepl("NOT_", data$sent.text2), ])/nrow(data) # Share


# Scoring
#--------

posterms <- rbind(sent.dictionary[sent.dictionary$sentiment == 1, ], neg.sent.dictionary[neg.sent.dictionary$sentiment == 1, 3:4])
negterms <- rbind(sent.dictionary[sent.dictionary$sentiment == -1, ], neg.sent.dictionary[neg.sent.dictionary$sentiment == -1, 3:4])


# Variable that stores occurrences of positive terms
data$posterms <- 0

for (i in 1:nrow(posterms)) {
  occur <- str_count(data$sent.text2, posterms$feature[i])
  data$posterms <- data$posterms + occur                  
}

# Variable that stores occurences of negative terms
data$negterms <- 0

for (i in 1:nrow(negterms)) {
  occur <- str_count(data$sent.text2, negterms$feature[i])
  data$negterms <- data$negterms + occur                      
}


# Calculate the raw sentiment score
data$sentiment <- data$posterms - data$negterms

# Normalized sentiment score
data$sentiment.norm <- data$sentiment / data$terms

# Remove temporary variables
# data$posterms <- data$neg.negterms <- data$negterms <- data$neg.posterms <- NULL

# Look at the distribution
hist(data$sentiment.norm, breaks =35)


# Saving the data
#################

save(data, file = "./4_CausaWulff/WulffCorpusScored.Rdata")

