#######################################################
# Project:  Validation of sentiment German dictionaries
#
# Task:     Test scoring exemplary text bits with the
#           augmented dictionary (using base R only)
#
# Author:   christian.rauh@wzb.eu / christian-rauh.eu 
# Date:     13.04.2017
######################################################



# Set YOUR working directory here (root of replication package)
setwd("M:/user/rauh/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication/")
setwd("C:/Users/CUZ/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication/")


# Load the augmented dictionary (Rauh)
#####################################

load("./1_Dictionaries/Rauh_SentDictionaryGerman.Rdata") 
load("./1_Dictionaries/Rauh_SentDictionaryGerman_Negation.Rdata") 


# TEST 1: Checking which terms account for the generated sentiment score
########################################################################

# The text to be scored
# Note: for pre-processing intricasies see main text section 3

text <- "Dieses famose Wörterbuch mach deutsche Sentimentanalyse ersr richtig reizvoll!"
# text <- "Das macht keinen Spass!"


# Clean text 
# Note: no stopword removal here

text2 <- text # Copy
text2 <- gsub("[0-9]", "", text2, fixed = F) # Remove numbers (assumption: they bear no sentiment)
text2 <- gsub("[[:punct:]]", "", text2, fixed = F) # Remove punctuation
text2 <- gsub("\\s+", " ", text2, fixed = F) # Reduce multiple consecutive spaces to exactly one regular whitespace
text2 <- paste(" ", text2, " ", sep = "") # Add whitespace left and right to ensure identification of leading and trailing terms
text2 <- tolower(text2) # To lower case

# Store number of terms in text bit

nterms <- (nchar(text2) - nchar(gsub(" ", "", text2, fixed = T))) - 1 # Number of terms inferred from number of whitespaces (minus the leading one)


# Loop over negation dictionary, and replace instances in text

for (i in 1:nrow(neg.sent.dictionary)){
  text2 <- gsub(neg.sent.dictionary$pattern[i], neg.sent.dictionary$replacement[i], text2, fixed = FALSE)
}

# Store which dictionary terms are found in example text

sent.dictionary$scored <- FALSE

for (i in 1:nrow(sent.dictionary)){
  sent.dictionary$scored[i] <- ifelse(grepl(sent.dictionary$feature[i], text2, fixed = TRUE),
                                      TRUE, sent.dictionary$scored[i])
}

neg.sent.dictionary$scored <- FALSE

for (i in 1:nrow(sent.dictionary)){
  neg.sent.dictionary$scored[i] <- ifelse(grepl(neg.sent.dictionary$feature[i], text2, fixed = TRUE),
                                      TRUE, neg.sent.dictionary$scored[i])
}

# Store detected terms
scored_terms <- rbind(sent.dictionary[sent.dictionary$scored == TRUE, ],
                 neg.sent.dictionary[neg.sent.dictionary$scored == TRUE, c("feature", "sentiment", "scored")])

# Length-normalized sentiment score for example text
sentiment.norm <- sum(as.integer(scored_terms$sentiment)) / nterms
