# Exercise 1: 

# You see some sentences extracted from Tom's and Monica's notebooks. 

text <- c(
  "Tom has visited 20 countries",
  "Tom only has eight fingers.",
  "Tom has worked at ten different jobs",
  "Monica can speak 6 languages",
  "tom's favorite food is pasta",
  "Tom can name 13 facts about himself.",
  "Tom's favorite two colors are orange and green",
  "Monica's favorite number is 8888.",
  "Tom lives at Leopoldstr. 12, Munich",
  "He is 8 feet tall"
)


# Print off each numeric number that is contained in the texts
stringr::str_extract(string = text, pattern = "\\d+")

# Find all items with a number followed by a space
stringr::str_extract(string = text, pattern = "\\d+\\s")

# How many times did they write down 'favorite'?
sum(stringr::str_detect(string = text, pattern = "favorite"))

# Exercise 2: 

# Create a regular expression that will match the mobile numbers as commonly written in Germany.

x <- c("0176-33665544", "(123)456-7890", "(123) 456-7890", "1235-2351")
stringr::str_view(x, "\\d\\d\\d\\d[[:punct:]]\\d\\d\\d\\d\\d\\d\\d\\d")
str_detect(string = x, pattern = "\\d\\d\\d\\d[[:punct:]]\\d\\d\\d\\d\\d\\d\\d\\d")
