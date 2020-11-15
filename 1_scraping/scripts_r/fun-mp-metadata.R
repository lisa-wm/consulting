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
#' @param chrome_version One of supported chrome versions to be used by
#' selenium driver
#' @param load_time Time in sec web driver will wait before clicking elements
#' (should be set higher for slow internet connections)
#' @param port Port to be used by selenium driver
#'
#' @return Object of class data.frame, containing relevant data per MP
#' 
get_mp_metadata <- function(chrome_version, port = 4567L, load_time = 5) {
  
  # Perform input checks
  
  if (!(chrome_version %in% unlist(binman::list_versions("chromedriver")))) {
    
    supported_versions <- unlist(binman::list_versions("chromedriver"))
    stop(
      sprintf(
        "Chrome version must be one of: %s",
        paste(supported_versions, collapse = ", ")))
  }
  
  # Set up selenium web driver
  
  driver <- set_up_selenium(chrome_version, port)
  
  # Scrape required data from website
  
  mp_df <- scrape_from_bt(driver, load_time)
  
  # Close down selenium driver
  
  driver$close()
  
}

# SUB-LEVEL FUNCTIONS ----------------------------------------------------------

# Set up selenium driver for web scraping

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

# Scrape required data from official Bundestag website

scrape_from_bt <- function(driver, load_time) {
  
  # Direct driver list of MPs
  
  jump_to_mp_list(driver, load_time)
  
  # Iterate over MPs and get relevant information
  
  mp_total <- 737
  
  mp_df <- data.frame(
    name = character(), 
    party = character(), 
    bundesland = character())

  for (mp in seq_len(737)) {
    
    mp_info <- NULL
    try(mp_info <- get_mp_info(mp, load_time), silent = TRUE)
    mp_df <- bind_rows(mp_df, mp_info)
    
  }  
  
  mp_df
  
}

# Navigate to list view of MPs on official Bundestag website

jump_to_mp_list <- function(driver, load_time) {
  
  url <- "https://www.bundestag.de/abgeordnete"
  driver$navigate(url)
  Sys.sleep(load_time)
  driver$maxWindowSize()
  
  list_button <- driver$findElement(
    using = "css selector",
    value = ".icon-list-bullet"
  )
  Sys.sleep(load_time)
  list_button$clickElement()
  
}

# Scrape data on MP level and convert to suitable format 

get_mp_info <- function(mp, load_time) {
  
  current_mp <- driver$findElement(
    using = "css selector",
    value = paste0("li:nth-child(", mp, ") .bt-person-fraktion")
  )
  Sys.sleep(3)
  current_mp$clickElement()
  
  name_party <- driver$findElement(
    using = "css selector",
    value = ".bt-biografie-name"
  )$getElementText()
  
  bundesland <- driver$findElement(
    using = "css selector",
    value = "#bt-landesliste-collapse h5"
  )$getElementText()
  
  name_party <- str_split(name_party, ", ", simplify = TRUE)
  name <- name_party[1]
  party <- str_split(name_party[2], "\\n", simplify = TRUE)[1]
  bundesland <- unlist(bundesland)
  
  jump_to_mp_list()
  
  data.frame(name, party, bundesland)
  
}


