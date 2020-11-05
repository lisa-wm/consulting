# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# WEB SCRAPING: OFFICIAL BUNDESTAG WEBSITE
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Purpose: Scrape data on MPs' names, party affiliations and electoral info

# Steps:
# 1. Set up selenium web driver
# 2. Scrape data

# ------------------------------------------------------------------------------
# STEP 1: SET UP SELENIUM DRIVER
# ------------------------------------------------------------------------------

# Check for currently supported chrome versions and choose the one compatible 
# with your browser (version browser >= version chromedriver)

(supported_chrome_versions <- unlist(binman::list_versions("chromedriver")))
my_chrome_version <- supported_chrome_versions[1]

# Kill any running sessions before firing up selenium

system("taskkill /im java.exe /f", intern = FALSE, ignore.stdout = FALSE)

# Instantiate driver

driver <- rsDriver(
  port = 4567L,
  browser = "chrome",
  chromever = my_chrome_version
)

# Instantiate remote driver for actual browsing

rem_driver <- driver[["client"]]

# ------------------------------------------------------------------------------
# STEP 2: SCRAPE DATA
# ------------------------------------------------------------------------------

# Define landing page

url <- "https://www.bundestag.de/abgeordnete"

# Direct remote driver to landing page

rem_driver$navigate(url)

# Select list view and wait for list to load

list_button <- rem_driver$findElement(
  using = "css selector", 
  value = ".icon-list-bullet"
)

Sys.sleep(3)

list_button$clickElement()

# Iterate over MPs and get relevant information

current_mp <- rem_driver$findElement(
  using = "css selector",
  value = "li:nth-child(1) .bt-person-fraktion"
)

Sys.sleep(3)

current_mp$clickElement()

mp_name_partei <- rem_driver$findElement(
  using = "css selector",
  value = ".bt-biografie-name"
)$getElementText()

mp_bundesland <- rem_driver$findElement(
  using = "css selector",
  value = "#bt-landesliste-collapse h5"
)$getElementText()

mp_wahlkreis_wknr_ <- rem_driver$findElement(
  using = "css selector",
  value = "#bt-landesliste-collapse .bt-link-intern"
)$getElementText()

mp_name <- unlist(
  strsplit(
    unlist(mp_name_partei), 
    ","
  )
)[1]

mp_partei <- unlist(
  strsplit(
    unlist(
      strsplit(unlist(mp_name_partei), ", "))[2],
    "\\n"
  )
)[1]

rem_driver$close()




