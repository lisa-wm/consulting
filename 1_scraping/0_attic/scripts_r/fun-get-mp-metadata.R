# ------------------------------------------------------------------------------
# SCRAPING GERMAN MPs' METADATA
# ------------------------------------------------------------------------------

# Purpose: collect German MPs' metadata (name, party affiliation, Bundesland)

# Steps:
# 1. Set up selenium web driver
# 2. Navigate to MP list on official Bundestag website
# 3. Iterate over all listed MPs and save name, party and Bundesland

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
  
  # Return data
  
  mp_df
  
}

# SUB-LEVEL FUNCTIONS ----------------------------------------------------------

# Scrape required data from official Bundestag website

scrape_from_bt <- function(driver, load_time) {
  
  # Direct driver list of MPs
  
  jump_to_mp_list(driver, load_time)
  
  # Iterate over MPs and get relevant information
  
  # !!! GET LENGTH OF LIST !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  a <- driver$findElements(
    using = "css selector",
    value = " .bt-person-fraktion"
  )
  mp_total <- 10
  # !!! GET LENGTH OF LIST !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  mp_range <- seq_len(mp_total)
  
  mp_df <- data.frame(
    name = character(), 
    party = character(), 
    bundesland = character(),
    twitter = character())

  for (mp in mp_range) {
    
    mp_info <- NULL
    try(mp_info <- get_mp_info(mp, load_time), silent = TRUE)
    mp_df <- bind_rows(mp_df, mp_info)
    mp_range <- c(nrow(mp_df), mp_total) # start over if failed
    
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
  name_party <- str_split(name_party, ", ", simplify = TRUE)
  name <- name_party[1]
  party <- str_split(name_party[2], "\\n", simplify = TRUE)[1]
  
  bundesland <- driver$findElement(
    using = "css selector",
    value = "#bt-landesliste-collapse h5"
  )$getElementText()
  bundesland <- unlist(bundesland)
  
  twitter <- NA
  try(
    {twitter <- driver$findElement(
      using = "css selector",
      value = "li~ li+ li .bt-link-extern"
    )$getElementAttribute("href")
    twitter <- unlist(twitter)},
    silent = TRUE
  )
  
  jump_to_mp_list(driver, load_time)
  
  data.frame(name, party, bundesland, twitter)
  
}

