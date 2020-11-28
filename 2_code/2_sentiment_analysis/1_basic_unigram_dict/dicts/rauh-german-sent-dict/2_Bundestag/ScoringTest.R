#####################################################
# Test scoring in individual text bits
#
# Project: Validation of sentiment dictionaries
#
# Author: christian.rauh@wzb.eu / christian-rauh.eu 
# Date:   13.04.2017
####################################################



# Set YOUR working directory here (root of replication package)
setwd("M:/user/rauh/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication/")
setwd("C:/Users/CUZ/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication/")


# Load the augmented dictionary (Rauh)
#####################################

load("./1_Dictionaries/Rauh_SentDictionaryGerman.Rdata") 
load("./1_Dictionaries/Rauh_SentDictionaryGerman_Negation.Rdata") 


# The text to be scored
# (note implementation intricasies in main text (section 3)!)
#############################################################

text <- " dieses famose Wörterbuch macht deutsche Sentiment Analyse erst richtig reizvoll "


# Scoring
#--------

# Clean text (stopword ermoval excluded here)

text2 <- text
text2 <- gsub("(\\.|\\?|!|:|,|;|-)", "", text2, fixed = FALSE)
text2 <- gsub("  ", " ", text2, fixed = FALSE)
text2 <- tolower(text2)

# Loop over negation dictionary, and replace instances in text

for (i in 1:nrow(neg.sent.dictionary)){
  text2 <- gsub(neg.sent.dictionary$pattern[i], neg.sent.dictionary$replacement[i], text2, fixed = FALSE)
}


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


results <- rbind(sent.dictionary[sent.dictionary$scored == TRUE, ],
                 neg.sent.dictionary[neg.sent.dictionary$scored == TRUE, c("feature", "sentiment", "scored")])

sentiment.norm <- sum(as.integer(results$sentiment)) / (nchar(gsub("[a-zäüöß]","", text2, fixed = FALSE))-1)


