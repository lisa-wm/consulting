###################################################
# Script prepares and exports sampled Bundestag 
# sentences for human validation survey
#
# Project: Validation of sentiment dictionaries
#
# Author: christian.rauh@wzb.eu / christian-rauh.eu 
# Date:   04.08.2015
###################################################

# Packages
library(stringr)

# Set YOUR working directory here (root of replication package)
setwd("M:/user/rauh/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication/")
setwd("C:/Users/CUZ/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication/")


# Load sampled sentences
########################

load(file = "./2_Bundestag/Data/ValidationSample.Rdata") # First Sample

# Clean up

sent.sample$sentence <- str_trim(sent.sample$sentence, side = "both") # Remove superfluous white spaces
sent.sample$sentence <- gsub("  ", " ", sent.sample$sentence, fixed = TRUE) # Remove superfluous white spaces


# Export test sheet for the survey app
######################################

first <- test.sample 

# Some cleaning, to make the text readable for human coders in shiny
first$sentence <- str_trim(first$sentence, side = "both")
first$sentence <- gsub(" ,", ",", first$sentence, fixed = TRUE)
first$sentence <- gsub(" :", ":", first$sentence, fixed = TRUE)
first$sentence <- gsub("  ", " ", first$sentence, fixed = TRUE)

first$sentence <- gsub("ß", "ss", first$sentence, fixed = TRUE)
first$sentence <- gsub("ä", "ae", first$sentence, fixed = TRUE)
first$sentence <- gsub("ö", "oe", first$sentence, fixed = TRUE)
first$sentence <- gsub("ü", "ue", first$sentence, fixed = TRUE)


# Set up sheet as used in the shiny survey app
# See: https://github.com/EconometricsBySimulation/Shiny-Demos/blob/master/Survey/Qlist.csv

first$Qnum <- first$sent_id # Unique sentence id used as question number in the app

first$Answer1 <- "Neutral" # Specify options for the human coders
first$Answer2 <- "Positiv"
first$Answer3 <- "Negativ"

names(first)[2] <- "Question" # Text fragment is the respective survey question

# Splitting into six pieces so as to keep the workload of the coders low

write.table(first[1:250 , c("Qnum", "Question", "Answer1", "Answer2", "Answer3")], file = "./2_Bundestag/Data/SurveyInput/Qlist1.csv", sep = ";", row.names = FALSE) # First sample
write.table(first[251:500 , c("Qnum", "Question", "Answer1", "Answer2", "Answer3")], file = "./2_Bundestag/Data/SurveyInput/Qlist2.csv", sep = ";", row.names = FALSE)
write.table(first[501:750 , c("Qnum", "Question", "Answer1", "Answer2", "Answer3")], file = "./2_Bundestag/Data/SurveyInput/Qlist3.csv", sep = ";", row.names = FALSE)
write.table(first[751:1000 , c("Qnum", "Question", "Answer1", "Answer2", "Answer3")], file = "./2_Bundestag/Data/SurveyInput/Qlist4.csv", sep = ";", row.names = FALSE)
write.table(first[1001:1250 , c("Qnum", "Question", "Answer1", "Answer2", "Answer3")], file = "./2_Bundestag/Data/SurveyInput/Qlist5.csv", sep = ";", row.names = FALSE)
write.table(first[1251:1500 , c("Qnum", "Question", "Answer1", "Answer2", "Answer3")], file = "./2_Bundestag/Data/SurveyInput/Qlist6.csv", sep = ";", row.names = FALSE)


