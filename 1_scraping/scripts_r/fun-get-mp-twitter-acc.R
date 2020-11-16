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
get_mp_twitter_accounts <- function(load_time = 5) {

  # Get all accounts from party websites
  
  bind_rows(
    get_party_twitter_accounts("cdu_csu"),
    get_party_twitter_accounts("fdp"),
    get_party_twitter_accounts("gruene"),
    get_party_twitter_accounts("linke"),
    get_party_twitter_accounts("spd")
  )
  
  # Get all from BT website
  # 
  # !!!!

}

# SUB-LEVEL FUNCTIONS ----------------------------------------------------------

get_party_twitter_accounts <- function(party) {
  
  # Retrieve list of HTML objects containing info per MP
  
  mp_list <- get_party_list(party)
  
  # Collect all names and Twitter accounts from selected party's website
  
  mp_df <- data.frame(
    name = character(), 
    link = character())

  for (mp in seq_along(mp_list)) {
    try({
      name <- get_party_names(party, mp_list, mp)
      link <- get_party_twitter_links(party, mp)
      mp_df <- bind_rows(mp_df, data.frame(name, link))
    })
  }
  
  mp_df
  
}

get_party_list <- function(party) {
  
  url <- switch(
    party,
    cdu_csu = paste0(
      "https://www.cducsu.de/hier-stellt-die-cducsu-bundestagsfraktion-ihre-",
      "abgeordneten-vor"),
    fdp = "https://www.fdpbt.de/koepfe",
    gruene = "https://www.gruene-bundestag.de/abgeordnete",
    linke = "https://www.linksfraktion.de/fraktion/abgeordnete/",
    spd = "https://www.spdfraktion.de/abgeordnete/alle?wp=19&view=list&old=19"
  )
  page_content <- read_html(url)
  
  page_content %>% html_nodes("ul > li:nth-child(1) > div > div.person-data-more > ul > li:nth-child(3) > a")
  
  switch(
    party,
    cdu_csu = page_content %>% 
      html_nodes("div.delegates-list > div > div > div"),
    fdp = "foo",
    gruene = "foo",
    linke = "foo",
    spd = "foo"
  )
}

get_party_names <- function(party, page_content, mp) {
  
  switch(
    party,
    cdu_csu = mp_list[[mp]] %>% 
      html_node("h2 > span") %>% 
      html_text(),
    fdp = "foo",
    gruene = "foo",
    linke = "foo",
    spd = "foo"
  )

}

get_party_twitter_links <- function(party, mp) {
  
  switch(
    party,
    cdu_csu = mp_list[[mp]] %>% 
      html_node("li.twitter > a") %>% 
      html_attr("href"),
    fdp = "foo",
    gruene = "foo",
    linke = "foo",
    spd = "foo"
  )

}

