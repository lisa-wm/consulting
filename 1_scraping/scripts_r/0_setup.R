# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# SET-UP: LOAD REQUIRED PACKAGES
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Purpose: Load all required packages for scripts

# ------------------------------------------------------------------------------
# PACKAGES 
# ------------------------------------------------------------------------------


my_type = ifelse(
  Sys.info()[["sysname"]] == "Linux", 
  "source", 
  "binary"
)

packages_required = c(
  "tidyverse",
  "xml2",
  "rvest"
)

not_installed = packages_required[!packages_required %in% 
                                    installed.packages()[, "Package"]]

if (length(not_installed) > 0) {
  
  lapply(
    not_installed,
    install.packages,
    repos = "http://cran.us.r-project.org",
    dependencies = TRUE,
    type = my_type
  )
  
}

lapply(packages_required, library, character.only = TRUE)