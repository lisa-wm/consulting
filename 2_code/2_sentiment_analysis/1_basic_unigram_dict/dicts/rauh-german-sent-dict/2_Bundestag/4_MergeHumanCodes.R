############################################################
# Combine individual human and autrmated sentiment codings 
# of 1,500 random sentences from plenary Bundestag speeches 
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


# Load automatically scored data
load("./2_Bundestag/Data/ValidationSampleScored.Rdata")

# Load human coded data
load("./2_Bundestag/Data/HumanCoding/HumanSentimentCodings.Rdata")

# Merge by sentence id (Qnum)
#----------------------------

data <- merge(sent.sample, human.codes, by = "Qnum", all = TRUE)
data$text <- NULL

# Save final data
#----------------

save(data, file = "./2_Bundestag/Data/ValidationSampleFinal.Rdata")

