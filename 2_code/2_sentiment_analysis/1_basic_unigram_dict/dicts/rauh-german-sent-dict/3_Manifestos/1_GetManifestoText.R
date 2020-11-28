############################################################
# Script retrives text fragments of directional coding items
# from the MANIFESTO database
#
# Project: Validation of sentiment dictionaries
#
# Author: christian.rauh@wzb.eu / christian-rauh.eu 
# Date:   13.04.2017
############################################################

library(manifestoR)
library(stringr)

# Set YOUR working directory here (root of replication package)
setwd("M:/user/rauh/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication/")
setwd("C:/Users/CUZ/WZB_CR/Datensaetze/SentiWS_Validation/JITP-Replication/")


# Set API key for Manifesto DB
# You have to register @ https://manifesto-project.wzb.eu/
# and then generate your personal API key on the profile page
mp_setapikey("manifesto_apikey.txt")


# List of CMP codes to be exploited for the test
################################################

# Essentially all classical categories with bi-directional options
# Hand picked from the MANIFESTO category scheme (v4) available at 
# https://manifesto-project.wzb.eu/coding_schemes/1 (27.07.2015)
categories <- read.table(file = "./Data/DirectionalManifestoCategories.csv", header = TRUE, sep = ";")


# Some cosmetics for the issue description
categories$issue[categories$issue = "Foreign Special Relationships"] <- "For. Special Relations"
categories$issue[categories$issue = "European Community/Union"] <- "European Union"
categories$issue[categories$issue = "Traditional Morality"] <- "Trad. Morality"
categories$issue[categories$issue = "National Way of Life"] <- "Nat. Way of Life"

# Vector of numerical CMP codes of interest
codes <- categories$cmp_code


# Download of MANIFESTO quasi-sentences based on CMP codes of interest
######################################################################

# Corpus version: 2016-3
all <- mp_corpus((countryname == "Austria" | countryname == "Germany" | countryname == "Switzerland") & edate > as.Date("1998-01-01"), codefilter = codes)

# Turn tm corpora into standard data frames
###########################################

# Empty data frame
all_df <- data.frame(text = character(), cmp_code = integer(), manifesto_id = character(), party = double(), date = double(), 
                         language = character(), stringsAsFactors = FALSE)

# Loop over objects in downloaded corpus
for (i in 1:length(all)){
  doc <- all[[i]] # Retrieve information from document i
  doc_df <- as.data.frame(doc, with.meta = TRUE) # Turn into data frame
  doc_df <- doc_df[ , c("text", "cmp_code", "manifesto_id" , "party", "date", "language")] # Keep only variables of interest, adapt to order of data frame
  all_df <- rbind(all_df, doc_df) # Combine
}


# Merge in category descriptions by CMP code
############################################

all_df_cat <- merge(all_df, categories, by = "cmp_code", all.x = TRUE)

# Keep only German language sentences
all_df_cat <- all_df_cat[all_df_cat$language == "german", ] 


# Descriptive overviews
#######################

# Available election dates
table(all_df_cat$date)

# Available parties
table(all_df_cat$party)

# Distribution of quasi sentences by category 
distr.over.cat <- as.data.frame(table(all_df_cat$issue))
distr.over.cat <- distr.over.cat[order(-distr.over.cat[, 2]), ] # Sort by frequency
write.table(distr.over.cat, file = "./3_Manifestos/Data/QuasiSentencesPerCategory.csv", sep = ";", row.names = FALSE)

# Distribution of quasi sentences by category and direction
distr.over.sub <- as.data.frame(table(all_df_cat$descr))
write.table(distr.over.sub, file = "./3_Manifestos/Data/QuasiSentencesPerSubCategory.csv", sep = ";",  row.names = FALSE)

# Average length in terms
all_df_cat$length <- str_count(all_df_cat$text, " ") + 1
mean.length <- aggregate(all_df_cat$length, by = list(all_df_cat$issue), FUN = mean)


# Store the data
################

de.manifesto <- all_df_cat
save(de.manifesto, file = "./3_Manifestos/Data/AT_DE_CH_Sample.Rdata")
