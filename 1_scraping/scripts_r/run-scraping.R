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

# Working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Required files

files_required <- list.files(pattern = "^fun-.*\\.R$")
sapply(files_required, source, .GlobalEnv)

# Packages

set_up_packages(packages_required = c(
  "tidyverse",
  "RSelenium"
  ))

# ------------------------------------------------------------------------------
# STEP 1: GET METADATA 
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# STEP 2: GET TWITTER ACCOUNTS 
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# STEP 3: GET TWEETS 
# ------------------------------------------------------------------------------

