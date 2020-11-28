############################################################
# Combine individual human sentiment codings of 1,500 random 
# sentences from plenary speeches in the German Bundestag 
# CHRISTIAN RAUH - 04.08.2015
############################################################


library(stringr)

setwd("C:/Users/CUZ/WZB_CR/Datensaetze/SentiWS_Validation/HumanValidation/HumanCoding") # CUZ Dell
setwd("M:/user/rauh/WZB_CR/Datensaetze/SentiWS_Validation/HumanValidation/HumanCoding") # WZB CR


# Coder 1 (Rebecca)
###################

# Switch folder
setwd(paste(getwd(), "/Coder1", sep = "")) 

load("survey.results.1.Rdata")
coder1.1 <- as.data.frame(t(presults)) # Transpose
coder1.1$Qnum <- row.names(coder1.1) # Sentence id - base for merge
coder1.1 <- coder1.1[which(coder1.1$Qnum != ""), ] # Nuissance created by the survey tool
coder1.1$Qnum <- as.integer(coder1.1$Qnum) # Turn into integers
coder1.1 <- coder1.1[order(coder1.1$Qnum), ] # Sort by sentence id
names(coder1.1)[1] <- "coder1"

load("survey.results.2.Rdata")
coder1.2 <- as.data.frame(t(presults)) # Transpose
coder1.2$Qnum <- row.names(coder1.2) # Sentence id - base for merge
coder1.2 <- coder1.2[which(coder1.2$Qnum != ""), ] # Nuissance created by the survey tool
coder1.2$Qnum <- as.integer(coder1.2$Qnum) # Turn into integers
coder1.2 <- coder1.2[order(coder1.2$Qnum), ] # Sort by sentence id
names(coder1.2)[1] <- "coder1"

load("survey.results.3.Rdata")
coder1.3 <- as.data.frame(t(presults)) # Transpose
coder1.3$Qnum <- row.names(coder1.3) # Sentence id - base for merge
coder1.3 <- coder1.3[which(coder1.3$Qnum != ""), ] # Nuissance created by the survey tool
coder1.3$Qnum <- as.integer(coder1.3$Qnum) # Turn into integers
coder1.3 <- coder1.3[order(coder1.3$Qnum), ] # Sort by sentence id
names(coder1.3)[1] <- "coder1"

load("survey.results.4.Rdata")
coder1.4 <- as.data.frame(t(presults)) # Transpose
coder1.4$Qnum <- row.names(coder1.4) # Sentence id - base for merge
coder1.4 <- coder1.4[which(coder1.4$Qnum != ""), ] # Nuissance created by the survey tool
coder1.4$Qnum <- as.integer(coder1.4$Qnum) # Turn into integers
coder1.4 <- coder1.4[order(coder1.4$Qnum), ] # Sort by sentence id
names(coder1.4)[1] <- "coder1"

load("survey.results.5.Rdata")
coder1.5 <- as.data.frame(t(presults)) # Transpose
coder1.5$Qnum <- row.names(coder1.5) # Sentence id - base for merge
coder1.5 <- coder1.5[which(coder1.5$Qnum != ""), ] # Nuissance created by the survey tool
coder1.5$Qnum <- as.integer(coder1.5$Qnum) # Turn into integers
coder1.5 <- coder1.5[order(coder1.5$Qnum), ] # Sort by sentence id
names(coder1.5)[1] <- "coder1"

load("survey.results.6.Rdata")
coder1.6 <- as.data.frame(t(presults)) # Transpose
coder1.6$Qnum <- row.names(coder1.6) # Sentence id - base for merge
coder1.6 <- coder1.6[which(coder1.6$Qnum != ""), ] # Nuissance created by the survey tool
coder1.6$Qnum <- as.integer(coder1.6$Qnum) # Turn into integers
coder1.6 <- coder1.6[order(coder1.6$Qnum), ] # Sort by sentence id
names(coder1.6)[1] <- "coder1"

rm(presults)

# Combine into one data frame
coder1 <- rbind(coder1.1, coder1.2, coder1.3, coder1.4, coder1.5, coder1.6)

# Remove individual surveya
rm(coder1.1, coder1.2, coder1.3, coder1.4, coder1.5, coder1.6)

# Switch folder
setwd(gsub("/Coder1", "", getwd(), fixed = TRUE)) # Switch folder


# Coder 2 (Felix)
#################

# Switch folder
setwd(paste(getwd(), "/Coder2", sep = "")) 

load("survey.results.1.Rdata")
coder2.1 <- as.data.frame(t(presults)) # Transpose
coder2.1$Qnum <- row.names(coder2.1) # Sentence id - base for merge
coder2.1 <- coder2.1[which(coder2.1$Qnum != ""), ] # Nuissance created by the survey tool
coder2.1$Qnum <- as.integer(coder2.1$Qnum) # Turn into integers
coder2.1 <- coder2.1[order(coder2.1$Qnum), ] # Sort by sentence id
names(coder2.1)[1] <- "coder2"

load("survey.results.2.Rdata")
coder2.2 <- as.data.frame(t(presults)) # Transpose
coder2.2$Qnum <- row.names(coder2.2) # Sentence id - base for merge
coder2.2 <- coder2.2[which(coder2.2$Qnum != ""), ] # Nuissance created by the survey tool
coder2.2$Qnum <- as.integer(coder2.2$Qnum) # Turn into integers
coder2.2 <- coder2.2[order(coder2.2$Qnum), ] # Sort by sentence id
names(coder2.2)[1] <- "coder2"

load("survey.results.3.Rdata")
coder2.3 <- as.data.frame(t(presults)) # Transpose
coder2.3$Qnum <- row.names(coder2.3) # Sentence id - base for merge
coder2.3 <- coder2.3[which(coder2.3$Qnum != ""), ] # Nuissance created by the survey tool
coder2.3$Qnum <- as.integer(coder2.3$Qnum) # Turn into integers
coder2.3 <- coder2.3[order(coder2.3$Qnum), ] # Sort by sentence id
names(coder2.3)[1] <- "coder2"

load("survey.results.4.Rdata")
coder2.4 <- as.data.frame(t(presults)) # Transpose
coder2.4$Qnum <- row.names(coder2.4) # Sentence id - base for merge
coder2.4 <- coder2.4[which(coder2.4$Qnum != ""), ] # Nuissance created by the survey tool
coder2.4$Qnum <- as.integer(coder2.4$Qnum) # Turn into integers
coder2.4 <- coder2.4[order(coder2.4$Qnum), ] # Sort by sentence id
names(coder2.4)[1] <- "coder2"

load("survey.results.5.Rdata")
coder2.5 <- as.data.frame(t(presults)) # Transpose
coder2.5$Qnum <- row.names(coder2.5) # Sentence id - base for merge
coder2.5 <- coder2.5[which(coder2.5$Qnum != ""), ] # Nuissance created by the survey tool
coder2.5$Qnum <- as.integer(coder2.5$Qnum) # Turn into integers
coder2.5 <- coder2.5[order(coder2.5$Qnum), ] # Sort by sentence id
names(coder2.5)[1] <- "coder2"

load("survey.results.6.Rdata")
coder2.6 <- as.data.frame(t(presults)) # Transpose
coder2.6$Qnum <- row.names(coder2.6) # Sentence id - base for merge
coder2.6 <- coder2.6[which(coder2.6$Qnum != ""), ] # Nuissance created by the survey tool
coder2.6$Qnum <- as.integer(coder2.6$Qnum) # Turn into integers
coder2.6 <- coder2.6[order(coder2.6$Qnum), ] # Sort by sentence id
names(coder2.6)[1] <- "coder2"

rm(presults)

# Combine into one data frame
coder2 <- rbind(coder2.1, coder2.2, coder2.3, coder2.4, coder2.5, coder2.6)

# Remove individual surveya
rm(coder2.1, coder2.2, coder2.3, coder2.4, coder2.5, coder2.6)

# Switch folder
setwd(gsub("/Coder2", "", getwd(), fixed = TRUE)) # Switch folder


# Coder 3 (Christian)
#####################

# Switch folder
setwd(paste(getwd(), "/Coder3", sep = "")) 

load("survey.results.1.Rdata")
coder3.1 <- as.data.frame(t(presults)) # Transpose
coder3.1$Qnum <- row.names(coder3.1) # Sentence id - base for merge
coder3.1 <- coder3.1[which(coder3.1$Qnum != ""), ] # Nuissance created by the survey tool
coder3.1$Qnum <- as.integer(coder3.1$Qnum) # Turn into integers
coder3.1 <- coder3.1[order(coder3.1$Qnum), ] # Sort by sentence id
names(coder3.1)[1] <- "coder3"

load("survey.results.2.Rdata")
coder3.2 <- as.data.frame(t(presults)) # Transpose
coder3.2$Qnum <- row.names(coder3.2) # Sentence id - base for merge
coder3.2 <- coder3.2[which(coder3.2$Qnum != ""), ] # Nuissance created by the survey tool
coder3.2$Qnum <- as.integer(coder3.2$Qnum) # Turn into integers
coder3.2 <- coder3.2[order(coder3.2$Qnum), ] # Sort by sentence id
names(coder3.2)[1] <- "coder3"

load("survey.results.3.Rdata")
coder3.3 <- as.data.frame(t(presults)) # Transpose
coder3.3$Qnum <- row.names(coder3.3) # Sentence id - base for merge
coder3.3 <- coder3.3[which(coder3.3$Qnum != ""), ] # Nuissance created by the survey tool
coder3.3$Qnum <- as.integer(coder3.3$Qnum) # Turn into integers
coder3.3 <- coder3.3[order(coder3.3$Qnum), ] # Sort by sentence id
names(coder3.3)[1] <- "coder3"

load("survey.results.4.Rdata")
coder3.4 <- as.data.frame(t(presults)) # Transpose
coder3.4$Qnum <- row.names(coder3.4) # Sentence id - base for merge
coder3.4 <- coder3.4[which(coder3.4$Qnum != ""), ] # Nuissance created by the survey tool
coder3.4$Qnum <- as.integer(coder3.4$Qnum) # Turn into integers
coder3.4 <- coder3.4[order(coder3.4$Qnum), ] # Sort by sentence id
names(coder3.4)[1] <- "coder3"

load("survey.results.5.Rdata")
coder3.5 <- as.data.frame(t(presults)) # Transpose
coder3.5$Qnum <- row.names(coder3.5) # Sentence id - base for merge
coder3.5 <- coder3.5[which(coder3.5$Qnum != ""), ] # Nuissance created by the survey tool
coder3.5$Qnum <- as.integer(coder3.5$Qnum) # Turn into integers
coder3.5 <- coder3.5[order(coder3.5$Qnum), ] # Sort by sentence id
names(coder3.5)[1] <- "coder3"

load("survey.results.6.Rdata")
coder3.6 <- as.data.frame(t(presults)) # Transpose
coder3.6$Qnum <- row.names(coder3.6) # Sentence id - base for merge
coder3.6 <- coder3.6[which(coder3.6$Qnum != ""), ] # Nuissance created by the survey tool
coder3.6$Qnum <- as.integer(coder3.6$Qnum) # Turn into integers
coder3.6 <- coder3.6[order(coder3.6$Qnum), ] # Sort by sentence id
names(coder3.6)[1] <- "coder3"

rm(presults)

# Combine into one data frame
coder3 <- rbind(coder3.1, coder3.2, coder3.3, coder3.4, coder3.5, coder3.6)

# Remove individual surveya
rm(coder3.1, coder3.2, coder3.3, coder3.4, coder3.5, coder3.6)

# Switch folder
setwd(gsub("/Coder3", "", getwd(), fixed = TRUE)) # Switch folder


# Combine all coders into one data set
######################################

human.codes <- merge(coder1, coder2, by = "Qnum", all = TRUE)
human.codes <- merge(human.codes, coder3, by = "Qnum", all = TRUE)            

names(human.codes) <- c("Qnum", "coder1", "coder2", "coder3")
rm(coder1, coder2, coder3)

human.codes$coder1 <- as.character(human.codes$coder1)
human.codes$coder2 <- as.character(human.codes$coder2)
human.codes$coder3 <- as.character(human.codes$coder3)


# Export reliability data for ReCal
###################################

reliability <- human.codes[ ,2:4]

reliability$coder1[reliability$coder1 == "Neutral"] <- 0
reliability$coder1[reliability$coder1 == "Positiv"] <- 1
reliability$coder1[reliability$coder1 == "Negativ"] <- 2

reliability$coder2[reliability$coder2 == "Neutral"] <- 0
reliability$coder2[reliability$coder2 == "Positiv"] <- 1
reliability$coder2[reliability$coder2 == "Negativ"] <- 2

reliability$coder3[reliability$coder3 == "Neutral"] <- 0
reliability$coder3[reliability$coder3 == "Positiv"] <- 1
reliability$coder3[reliability$coder3 == "Negativ"] <- 2

reliability$coder1 <- as.numeric(reliability$coder1)
reliability$coder2 <- as.numeric(reliability$coder2)
reliability$coder3 <- as.numeric(reliability$coder3)

write.table(reliability, file = "ReliabilityData.csv", col.names = FALSE, row.names = FALSE, sep = ",")


# Get the majority vote
#######################

human.codes$help <- paste(human.codes$coder1, human.codes$coder2, human.codes$coder3, sep = " ")
human.codes$posvotes <- str_count(human.codes$help, "Positiv")
human.codes$negvotes <- str_count(human.codes$help, "Negativ")

human.codes$vote <- "Neutral"
human.codes$vote[human.codes$posvotes == 3] <- "Clearly positive"
human.codes$vote[human.codes$posvotes == 2] <- "Rather positive"
human.codes$vote[human.codes$negvotes == 3] <- "Clearly negative"
human.codes$vote[human.codes$negvotes == 2] <- "Rather negative"

human.codes$help <- human.codes$posvotes <- human.codes$negvotes <- NULL # Remove temporary variables

# Store the data frame
######################

save(human.codes, file = "HumanSentimentCodings.Rdata")

