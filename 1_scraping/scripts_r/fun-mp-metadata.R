# ------------------------------------------------------------------------------
# SCRAPING GERMAN MPs' METADATA
# ------------------------------------------------------------------------------

# Purpose: collect German MPs' metadata (name, party affiliation, electoral
# district)

# Steps:
# 1. Set up selenium web driver

# TOP-LEVEL FUNCTION -----------------------------------------------------------

#' Get MP metadata from official Bundestag website
#'
#' @param chrome_version 
#' @param port 
#'
#' @return
#' @export
#'
#' @examples
#' 
get_mp_metadata <- function(chrome_version, port = 4567L) {
  
  # Perform input checks
  
  if (!(chrome_version %in% unlist(binman::list_versions("chromedriver")))) {
    
    supported_versions <- unlist(binman::list_versions("chromedriver"))
    stop(
      sprintf(
        "Chrome version must be one of: %s",
        paste(supported_versions, collapse = ", ")))
  }
  
  # Set up selenium web driver
  
  my_driver <- set_up_selenium(chrome_version, port)
  
  # Scrape required data from website
  
  scrape_from_bt(my_driver)
  
}

# SUB-LEVEL FUNCTIONS ----------------------------------------------------------

#' Set up selenium driver for web scraping
#'
#' @param chrome_version One of supported chrome versions to be used by
#' selenium driver
#' @param port Port to be used by selenium driver
#'
#' @return Starts a chrome browser for remote control
#' 
set_up_selenium <- function(chrome_version, port) {
  
  # Kill any running sessions before firing up selenium
  
  system("taskkill /im java.exe /f", intern = FALSE, ignore.stdout = FALSE)
  
  # Instantiate driver
  
  driver <- rsDriver(
    port = port,
    browser = "chrome",
    chromever = chrome_version
  )
  
  # Instantiate remote driver for actual browsing
  
  driver[["client"]]
  
}


#' Scrape required data from official Bundestag website
#'
#' @param driver Object of class RSelenium
#'
#' @return Object of class data.frame containing MPs' names and metadata
#' 
scrape_from_bt <- function(driver) {
  
  # Define landing page
  
  url <- "https://www.bundestag.de/abgeordnete"
  
  # Direct remote driver to landing page
  
  driver$navigate(url)
  
  Sys.sleep(5)
  
  # Select list view and wait for list to load
  
  list_button <- driver$findElement(
    using = "css selector",
    value = ".icon-list-bullet"
  )
  
  Sys.sleep(5)
  
  list_button$clickElement()
  
  # Iterate over MPs and get relevant information
  
  # for (in MPs) collect info
  
}

get_mp_info <- function(i) {
  
  current_mp <- driver$findElement(
    using = "css selector",
    value = "li:nth-child(1) .bt-person-fraktion"
  )
  
  Sys.sleep(3)
  
  current_mp$clickElement()
  
  mp_name_party <- driver$findElement(
    using = "css selector",
    value = ".bt-biografie-name"
  )$getElementText()
  
  mp_bundesland <- driver$findElement(
    using = "css selector",
    value = "#bt-landesliste-collapse h5"
  )$getElementText()
  
  mp_district_nr <- driver$findElement(
    using = "css selector",
    value = "#bt-landesliste-collapse .bt-link-intern"
  )$getElementText()
  
  mp_name_party <- str_split(mp_name_party, ", ", simplify = TRUE)
  mp_name <- mp_name_party[1]
  mp_party <- str_split(mp_name_party[2], "\\n", simplify = TRUE)[1]
  
  mp_district <- unlist(strsplit(unlist(mp_district_nr), " "))[2]
  
  driver$close()
  
  data.frame(
    mp_name = mp_name_party[1],
    mp_party,
    mp_bundesland,
    mp_electoral
  )
  
}