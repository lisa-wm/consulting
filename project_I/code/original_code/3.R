# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Preparation -----------------------------------------
# ----------------------------------------------------------------------------------------------

# Install and load required packages
os <- Sys.info()[["sysname"]] # Get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # Set corresponding installation type
packages_required <- c(
  "betareg", "ggcorrplot", "grid", "gridExtra", "huge", "knitr", "mvtnorm", 
  "quanteda", "reshape2", "scales", "stm", "stringi", "tidyverse", "tm"
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

# set working directory (to folder where this code file is saved)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# data
## load twitter data and aggregate on a per-user basis
topic <- readRDS("../data/topic_preparation/topic.rds")
prep <- readRDS("../data/topic_preparation/prep.rds")

topic_user <- topic %>% 
  group_by(Name) %>% 
  mutate(Tweets_Dokument = paste(Tweets, collapse = ' ')) %>%
  summarize(
    Twitter_Username = max(Twitter_Username), 
    Tweets_Dokument = max(Tweets_Dokument),
    Anzahl_Follower = max(Anzahl_Follower)
  ) %>%
  ungroup()

## merge with personal and socioeconomic data
abg_df <- read_delim("../data/web_scraping/abg_df.csv", delim = ",") %>%
  rename(Twitter_Username = Twitter, Wahlkreis_Nr = `Wahlkreis-Nr.`)
se_df <- read_delim("../data/web_scraping/se_df.csv", delim = ",") %>% 
  rename(Wahlkreis_Nr = `Wahlkreis-Nr.`, "AfD Anteil" = "AFD Anteil") %>% 
  select(-Bundesland)

topic_user_merged <- topic_user %>% 
  inner_join(abg_df) %>% 
  inner_join(se_df, by = "Wahlkreis_Nr") %>%
  filter(Partei != "fraktionslos") # dropping independent MPs (only one)

alldata <- topic %>% 
  inner_join(abg_df) %>% 
  inner_join(se_df, by = "Wahlkreis_Nr") %>%
  filter(Partei != "fraktionslos") # dropping independent MPs (only one)

nrow(topic) # initial number of tweets
nrow(topic_user) # number of MPs included at this stage
nrow(topic_user) - nrow(topic_user_merged) - 1 # number of MPs without a electoral district information available
nrow(prep) # number of remaining MPs after removing those without electoral-district info and the one independent MP
scales::percent(nrow(prep)/709) # percentage of all 709 MPs included in further analysis
nrow(alldata) # number of tweets of remaining MPs

# total monthly tweet frequencies for our period of analysis (2017-09-24 through 2020-04-24)
## get date variable on x-axis
data_adj <- alldata %>% mutate(Jahr = lubridate::year(Datum), Monat = lubridate::month(Datum))
data_adj$date <- with(data_adj, sprintf("%d-%02d", Jahr, Monat))
# plot
ggplot(data_adj, aes(x = date)) + geom_histogram(stat = "count") +
  labs(x = "time", y = "# tweets")  +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

# load monthly data post preprocessing
data <- readRDS("../data/topic_preprocessing/preprocessed_monthly.rds")

nrow(data$meta) # MP-level documents (monthly!) remaining after preprocessing
ncol(data$meta) # number of covariates
