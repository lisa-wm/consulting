###########################################################
# Calculate different sentiment scores for a random sample
# of 1,500 sentences from plenary speeches in the German 
# Bundestag between 1991 and 2013
#
# Project: Validation of sentiment dictionaries
#
# Author: christian.rauh@wzb.eu / christian-rauh.eu 
# Date:   06.08.2015
###########################################################


# Packages
library(stringr)

# Set YOUR working directory here (root of replication package)
setwd("M:/user/rauh/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication/")
setwd("C:/Users/CUZ/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication/")


# Load sampled sentences
########################

load(file = "./2_Bundestag/Data/ValidationSample.Rdata")

sent.sample$sentence <- str_trim(sent.sample$sentence, side = "both") # Remove superfluous white spaces
sent.sample$sentence <- gsub("  ", " ", sent.sample$sentence, fixed = TRUE) # Remove superfluous white spaces

names(sent.sample)[1] <- "Qnum" # Same name as in Human Validation Survey

# Get length (number of terms) of each text segment
sent.sample$help <- gsub(", |: |\\. |\\? |! |- ", "", sent.sample$sentence, fixed = FALSE) # Remove punctuation so as not to distort the word count
sent.sample$terms <- str_count(sent.sample$help, " ") + 1  # Number of terms based on number of spaces
sent.sample$help<- NULL

hist(sent.sample$terms, breaks = 200, main = "Length of sentences in sample")
summary(sent.sample$terms)

# Add exactly one whitespace left and right (ensure identification of full terms)
sent.sample$sentence <- paste(" ", sent.sample$sentence, " ", sep = "")

# Stopword removal from term count: 3_SentimentScoring_4_MAN.R



# Sentiment Scoring: SentiWS dictionary
#######################################


# Load and prepare the dicitionary
#---------------------------------

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

sent.sample$sentence2  <- sent.sample$sentence # Copy of the sampled sentences

for (i in 1:nrow(posterms)) {
  sent.sample$sentence2 <- gsub(posterms$inflections[i], posterms$term[i], sent.sample$sentence2, fixed = FALSE)
}

for (i in 1:nrow(negterms)) {
  sent.sample$sentence2 <- gsub(negterms$inflections[i], negterms$term[i], sent.sample$sentence2, fixed = FALSE)
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

sent.sample$sentiws <- 0

# Postive terms, increase count by number of occurences

for (i in 1:nrow(posterms)) {
  occur <- str_count(sent.sample$sentence2, posterms$term[i])
  sent.sample$sentiws <- sent.sample$sentiws + occur                     
}

# Negative terms, decrease count by number of occurences

for (i in 1:nrow(negterms)) {
  occur <- str_count(sent.sample$sentence2, negterms$term[i])
  sent.sample$sentiws <- sent.sample$sentiws - occur                      
}

# Normalize along length of scored text
sent.sample$sentiws.norm <- sent.sample$sentiws / sent.sample$terms

# Remove SENTI WS dictionary from workspace
rm("posterms" , "negterms")

# Remove pre-processed text
sent.sample$sentence2 <- NULL



# Sentiment Scoring: German Polarity Clues
###########################################

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

sent.sample$polarity <- 0

for (i in 1:nrow(posterms)) {
  occur <- str_count(sent.sample$sentence, posterms$term[i])
  sent.sample$polarity <- sent.sample$polarity + occur                  
}

for (i in 1:nrow(negterms)) {
  occur <- str_count(sent.sample$sentence, negterms$term[i])
  sent.sample$polarity <- sent.sample$polarity - occur                      
}


# Normalize along length of scored text
sent.sample$polarity.norm <- sent.sample$polarity / sent.sample$terms

# Remove Polarity Clues dictionary from workspace
rm("posterms" , "negterms")


# The augmented dictionary (Rauh)
#################################

load("./1_Dictionaries/Rauh_SentDictionaryGerman.Rdata") 
load("./1_Dictionaries/Rauh_SentDictionaryGerman_Negation.Rdata") 


# Marking directly negated sentiment features in the source text
#---------------------------------------------------------------

sent.sample$text <- sent.sample$sentence # Copy of the sampled sentences

# Loop over negation dictionary, and replace instances in text
for (i in 1:nrow(negation)){
  sent.sample$text <- gsub(negation$pattern[i], negation$replacement[i], sent.sample$text, fixed = FALSE)
}

# How many instances were affected?
nrow(sent.sample[grepl("NOT_", sent.sample$text), ]) # Abs
nrow(sent.sample[grepl("NOT_", sent.sample$text), ])/nrow(sent.sample) # Share


# Scoring
#--------

posterms <- sent.dictionary[sent.dictionary$sentiment == 1, ]
negterms <- sent.dictionary[sent.dictionary$sentiment == -1, ]


# Variable that stores occurrences of positive terms
sent.sample$posterms <- 0

for (i in 1:nrow(posterms)) {
  occur <- str_count(sent.sample$text, posterms$feature[i])
  sent.sample$posterms <- sent.sample$posterms + occur                  
}

# Variable that stores occurences of negative terms
sent.sample$negterms <- 0

for (i in 1:nrow(negterms)) {
  occur <- str_count(sent.sample$text, negterms$feature[i])
  sent.sample$negterms <- sent.sample$negterms + occur                      
}


# Calculate the raw sentiment score
sent.sample$sentiment <- sent.sample$posterms - sent.sample$negterms

# Normalized sentiment score
sent.sample$sentiment.norm <- sent.sample$sentiment / sent.sample$terms

# Remove temporary variables
# sent.sample$posterms <- sent.sample$neg.negterms <- sent.sample$negterms <- sent.sample$neg.posterms <- NULL


# Saving
########
save(sent.sample, file = "./2_Bundestag/Data/ValidationSampleScored.Rdata")
