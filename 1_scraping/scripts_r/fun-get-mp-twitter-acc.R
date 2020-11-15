# ------------------------------------------------------------------------------
# SCRAPING GERMAN MPs' TWITTER ACCOUNT NAMES
# ------------------------------------------------------------------------------

# Purpose: find Twitter account names for all MPs

# Steps:
# 1. 

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
get_mp_twitter_acc <- function(chrome_version, port = 4567L, load_time = 5) {

  driver <- set_up_selenium(chrome_version, port)
  
  acc_cdu_csu <- get_party_acc(
    driver = driver,
    url = paste0(
      "https://www.cducsu.de/hier-stellt-die-cducsu-bundestagsfraktion",
      "-ihre-abgeordneten-vor"),
    load_time = load_time,
    party_scrape_fun = get_cdu_csu_acc
  )
    
    
}

# SUB-LEVEL FUNCTIONS ----------------------------------------------------------

get_party_acc <- function(driver, url, load_time, party_scrape_fun) {
  
  driver$navigate(url)
  Sys.sleep(load_time)
  driver$maxWindowSize()
  
  mp_total <- 20
  
  mp_df <- data.frame(
    name = character(), 
    acc_link = character())
  
  for (mp in seq_len(mp_total)) {
    
    mp_acc <- NULL
    try(mp_acc <- party_scrape_fun(mp, load_time), silent = TRUE)
    mp_df <- bind_rows(mp_df, mp_acc)
    
  }  
  
  mp_df
  
}

get_cdu_csu_acc <- function(mp, driver, load_time) {
  
name <- driver$findElement(
  using = "css selector",
  value = 
    paste0(
      ".delegates-list-block:nth-child(",
      mp,
      ") .delegates:nth-child(1)")
)$getElementText()
name <- str_split(name, "\\n", simplify = TRUE)[1]

acc_link <- driver$findElement(
  using = "css selector",
  value = paste0(
    ".delegates-list-block:nth-child(",
    mp,
    ") .delegates:nth-child(1) .twitter a")
)$getElementAttribute("href")
acc_link <- unlist(acc_link)

data.frame(name, acc_link)

}
