# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# WEB SCRAPING: OFFICIAL BUNDESTAG WEBSITE
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Purpose: Scrape data on MPs' names, party affiliations and electoral info

# ------------------------------------------------------------------------------
# PREREQUISITES 
# ------------------------------------------------------------------------------

# Working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Required files

source("0_setup.R")

# ------------------------------------------------------------------------------
# XXXX 
# ------------------------------------------------------------------------------

url <- paste0(
  "https://web.archive.org/web/20190202054736/",
  "https://www.boxofficemojo.com/movies/?id=ateam.htm"
)
ateam <- read_html(url)
ateam %>% html_nodes("center")

ateam$node

url = "https://www.bundestag.de/abgeordnete"
bt_html = read_html(url)
bt_html$node

sesh = html_session("https://www.bundestag.de/abgeordnete")

sesh %>% 
  jump_to()











