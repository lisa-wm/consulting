# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# SCRAPING TWITTER DATA FOR GERMAN MPs
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Purpose: Collect German MPs' tweets for analysis

# Steps:
# 1. Get metadata
# 2. Get Twitter accounts
# 3. Get tweets

# ------------------------------------------------------------------------------
# PREREQUISITES 
# ------------------------------------------------------------------------------

# PACKAGES ---------------------------------------------------------------------

packages_required <-  c(
  "here",
  "tidyverse",
  "RSelenium"
)

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

# SOURCE FILES -----------------------------------------------------------------

# files_required <- list.files(pattern = "^fun-.*\\.R$")
# sapply(files_required, source, .GlobalEnv)

source(here("1_scraping/scripts_r", "fun-setup.R"))

# ------------------------------------------------------------------------------
# STEP 1: GET METADATA 
# ------------------------------------------------------------------------------

# Fire up selenium driver

remmi_demmi <- set_up_selenium()
(testitest <- scrape_from_bt(rem_driver = remmi_demmi)) # list icon problem

# ------------------------------------------------------------------------------
# STEP 2: GET TWITTER ACCOUNTS 
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# STEP 3: GET TWEETS 
# ------------------------------------------------------------------------------

