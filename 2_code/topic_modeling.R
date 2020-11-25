
# TOPIC MODELING  ---------------------------------------------------------
# based on PROJECT I ------------------------------------------------------


# Install and load required packages
os <- Sys.info()[["sysname"]] # Get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # Set corresponding installation type
packages_required <- c(
  "quanteda", "stringi", "tidyverse"
)
not_installed <- packages_required[!packages_required %in%
                                     installed.packages()[, "Package"]]
if (length(not_installed) > 0) {
  lapply(
    not_installed,
    install.packages,
    repos = "http://cran.us.r-project.org",
    dependencies = TRUE,
    type = itype
  )
}
lapply(packages_required, library, character.only = TRUE)

setwd("D:/UNI/01_Master/3. Semester/Consulting/consulting/project_I/code")

# The section below is commented out because otherwise all the files in the output folder
# will be overwritten.
# The input-file twitter_df.csv in the input folder has to be updated first!

# # Prepare Twitter data
# source("./topic_preparation.R")
# 
# # Prepare Twitter data (monthly, monthly train, monthly test)
# source("./topic_preprocessing.R")
# 
# # Preprocess Monthly Twitter data
# source("./3.R")
# 
# # Hyperparameter optimizing and model fitting (ATTENTION! This takes long...)
source("./4.R")


# Modification outside of the earlier project -----------------------------

# match raw tweets with main data
matched_topics <-  topic_props %>%
  #arrange(desc(!!as.symbol(topic_number_long))) %>% # order by topic proportion of selected topic, in decreasing order
  .[, c("Name", "docname", "Datum", "Partei", "Bundesland", paste0("Topic", 1:15))] %>% # select variables from main data to be included in output table
  left_join(data_corpus[,c("Tweets_Dokument", "docname")], # select variables from data_corpus to be included
            by = "docname")

# Assign one Topic to each tweet. The one with maximal score (maybe include weights based on the score value)
exctract_max_score <- matched_topics %>% select(paste0("Topic", 1:15)) %>% mutate(Max_Topic_Score = invoke(pmax, na_if(., 0), na.rm = TRUE))
exctract_max_topic <- colnames(matched_topics %>% select(paste0("Topic", 1:15)))[apply(matched_topics %>% select(paste0("Topic", 1:15)),1,which.max)]

topic_labels <- list(
  Topic1 = "Right/Nationalist",
  Topic2 = "Miscellaneous 1",
  Topic3 = "Climate Economics",
  Topic4 = "Social/Housing",
  Topic5 = "Digital/Future",
  Topic6 = "Climate Protection",
  Topic7 = "Europe",
  Topic8 = "Corona",
  Topic9 = "Left/Anti-war",
  Topic10 = "Twitter/Politics 1",
  Topic11 = "Twitter/Politics 2",
  Topic12 = "Miscellaneous 2",
  Topic13 = "Twitter/Politics 3",
  Topic14 = "Right-wing Extremism",
  Topic15 = "Society/Solidarity"
)

matched_topics <- matched_topics %>% mutate(Max_Score = exctract_max_score$Max_Topic_Score,
                                            Max_Topic_Nr = exctract_max_topic)

levels(matched_topics$Max_Topic)
matched_topics$Max_Topic_Label <- as.character(topic_labels[matched_topics$Max_Topic_Nr])

matched_topics <- matched_topics %>% select(-paste0("Topic", 1:15))

