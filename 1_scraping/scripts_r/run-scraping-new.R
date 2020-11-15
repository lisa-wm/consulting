# ------------------------------------------------------------------------------
# SCRAPING TWITTER DATA FOR GERMAN MPs
# ------------------------------------------------------------------------------

# Purpose: collect German MPs' tweets for analysis

# Steps:
# 1. Get metadata
# 2. Get Twitter accounts
# 3. Get tweets

# PREREQUISITES ----------------------------------------------------------------

# Install, if necessary, and load required packages

packages_required <-  c(
  "here",
  "tidyverse",
  "RSelenium"
)

set_up_packages <- function(pkg) {
  
  my_type <- ifelse(
    Sys.info()[["sysname"]] == "Linux", 
    "source", 
    "binary"
  )
  
  not_installed <- 
    packages_required[!packages_required %in% installed.packages()[, "Package"]]
  
  if (length(not_installed) > 0) {
    
    lapply(
      not_installed,
      install.packages,
      repos = "http://cran.us.r-project.org",
      dependencies = TRUE,
      type = my_type
    )
    
  }
  
  lapply(packages_required, library, character.only = TRUE, quietly = TRUE)
}

invisible(set_up_packages(packages_required))

# Source required files containing sub-level functions

files_required <- list(
  here("1_scraping/scripts_r", "fun-set-up-selenium.R"),
  here("1_scraping/scripts_r", "fun-get-mp-metadata.R")
)
invisible(sapply(files_required, source, .GlobalEnv))

# STEP 1: GET METADATA ---------------------------------------------------------

# !!! AUTOMATE THIS !!!
# maybe using system2()
(supported_chrome_versions <- unlist(binman::list_versions("chromedriver")))
chrome_version <- supported_chrome_versions[1]

mp_df <- get_mp_metadata(...)

# STEP 2: GET TWITTER ACCOUNTS -------------------------------------------------

mp_twitter_df <- get_mp_twitter_acc(...)

# STEP 3: GET TWEETS -----------------------------------------------------------

mp_tweets_df <- get_mp_tweets(...)
