
####################################################
# Script draws a random sample of 1,500 sentences 
# from the full-text Bundestag corpus presented in
# Rauh (2015) EUP
#
#
# Project: Validation of sentiment dictionaries
#
# Author: christian.rauh@wzb.eu / christian-rauh.eu 
# Date:   22.07.2015
###################################################

# Packages
library(stringr)


# Set YOUR working directory here (root of replication package)
setwd("M:/user/rauh/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication/")
setwd("C:/Users/CUZ/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication/")


# Load BT corpus
################
# Please contact me via christian.rauh@wzb.eu if you require the full text Bundestag corpus
load("BTCorpusWP12-17-Eutagged.Rdata")


# Sampling procedure 
####################


# Sampling 1500 speeches
#----------------------

set.seed(27062011) # To make this replicable
speech.sample <- btcorpus[sample(nrow(btcorpus), 1500), ] # Random draw of 1500 speeches


# Sampling sentences
#-------------------

# Variable that stores the number of sentences in each speech
# Based on sentence end markers
# Note that the text in the original corpus was edited such that punctuation is always preceeded by a whitespace

# Some additional edits are necessary to make sure that punctuation only refers to sentence ending

speech.sample$text <- paste(speech.sample$text, " .", sep = "") # Make sure that each speech ends with a dot

speech.sample$text <- str_replace_all(speech.sample$text, " \\. \\.", " .") # Doubled sentence end markers
speech.sample$text <- gsub("([0-9])( )(\\.)", "\\1\\3", speech.sample$text) # Ordered numbers (wrong positives) as in " 3 . august" are replace by "3. august"

speech.sample$text <- gsub(" bzw .", " bzw.", speech.sample$text, fixed = TRUE) # Ordinary German abbreviation (wrong positive)
speech.sample$text <- gsub(" art .", " art.", speech.sample$text, fixed = TRUE) # Ordinary German abbreviation (wrong positive)
speech.sample$text <- gsub(" z . T .", " z.T.", speech.sample$text, fixed = TRUE) # Ordinary German abbreviation (wrong positive)
speech.sample$text <- gsub(" parl .", " parl.", speech.sample$text, fixed = TRUE) # Ordinary German abbreviation (wrong positive)
speech.sample$text <- gsub(" abs .", " abs.", speech.sample$text, fixed = TRUE) # Ordinary German abbreviation (wrong positive)
speech.sample$text <- gsub(" nr .", " nr.", speech.sample$text, fixed = TRUE) # Ordinary German abbreviation (wrong positive)
speech.sample$text <- gsub(" u . a .", " u.a.", speech.sample$text, fixed = TRUE) # Ordinary German abbreviation (wrong positive)
speech.sample$text <- gsub(" d . h .", " d.h.", speech.sample$text, fixed = TRUE) # Ordinary German abbreviation (wrong positive)
speech.sample$text <- gsub(" z . b .", " z.b.", speech.sample$text, fixed = TRUE) # Ordinary German abbreviation (wrong positive)

speech.sample$text <- gsub(" _ ", " - ", speech.sample$text, fixed = TRUE) # Formatting issue, just not to distract the coders

# Count sentences based on punctuation

speech.sample$sentences <- str_count(speech.sample$text, "[A-Za-z????]( \\.| \\?| !)")


# Random sentence number per speech

for (i in 1:1500){
  speech.sample$sent_samp[i] <- sample.int(n = speech.sample$sentences[i], size = 1) # Random integer smaller or equal to number of senteces in speech
  }

length(speech.sample$text[speech.sample$sentences == 0]) # checking for errors

# Extract the sampled sentence
#-----------------------------

speech.sample$sample_sentence <- speech.sample$text # Copy text of full speech

# For each speech
for (i in 1:1500){
  
  # Repeat until xth sentence is reached
  for (j in 1:(speech.sample$sent_samp[i]-1)){
    
    # Remove everything until first occurence of sentence end marker
    speech.sample$sample_sentence[i] <- str_replace(speech.sample$sample_sentence[i], ".*?( \\.| \\?| !)", "")    
  }  
  
  # Remove then everything after this sentence
  speech.sample$sample_sentence[i] <- str_replace(speech.sample$sample_sentence[i], "( \\.| \\?| !).*", "") 
}


# This results in an empty cell, where the speech was only one sentence long, we correct for this here
speech.sample$sample_sentence <- ifelse(speech.sample$sentences == 1,
                                        speech.sample$text,
                                        speech.sample$sample_sentence)

# It also results in an empty cell, in case where the speech was 2 sentences long and the first sentence should be sampled
speech.sample$sample_sentence <- ifelse(speech.sample$sentences == 2 & speech.sample$sent_samp == 1,
                                        str_replace(speech.sample$text, "( \\.| \\?| !).*", "") , # Keeps only first sentence
                                        speech.sample$sample_sentence) # Otherwise stay as it is


# Store the final sentence sample
#--------------------------------

sent.sample <- as.data.frame(speech.sample$sample_sentence)
names(sent.sample)[1] <- "sentence"

sent.sample$sent_id <- row.names(sent.sample) # Create unique ID for each sentence (to later match it with the human codes)

sent.sample <- sent.sample[ , c("sent_id", "sentence")]

save(sent.sample, file = "./2_Bundestag/Data/ValidationSample.Rdata") # save
