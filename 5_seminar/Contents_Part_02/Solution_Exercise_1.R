library(rvest)

# Exercise
Example1 <- read_html("https://practicewebscrapingsite.wordpress.com/example-1")

# Extract title info
title <- Example1 %>% 
  html_nodes("strong") %>%
  html_text()

# Extract text info
text <- Example1 %>% 
  html_nodes(".Content") %>%
  html_text()

# Save in a data frame
Example1_df <- data.frame(title = title,
           text  = text)
