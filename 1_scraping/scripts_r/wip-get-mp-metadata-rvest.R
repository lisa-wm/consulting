url <- "https://www.bundestag.de/abgeordnete"
page_content <- read_html(url)

a <- page_content %>% 
  html_nodes("a > div > div > p")
a
