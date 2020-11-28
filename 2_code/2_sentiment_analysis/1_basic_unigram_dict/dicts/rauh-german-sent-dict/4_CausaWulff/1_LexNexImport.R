###############################################################
# Script turns cleans raw texts retrived from LexNexis
# and turns them into a data frame, one observation per article
# 
#
# Project: Validation of sentiment dictionaries
#
# Author: christian.rauh@wzb.eu / christian-rauh.eu 
# Date:   28.03.2017
###################################################

Sys.time()

# Packages
library(stringr) # 1.0.0

# Working directory - ADAPT TO YOUR ENVIRONMENT
setwd("C:/Users/CUZ/WZB_CR/Datensaetze/SentiWS_Validation/CausaWulff")


# Establish Importing/ Cleaning function
#######################################

LexClean <- function (path) { # function that expects the path with the TXT files retrieved from Lexis Nexis

  # Importing the text
  #-------------------
  
  setwd(path)
  
  # Reading all txt-files within PATH, individual lines as vectors
  # Collapsing to a single string, then combine to one corpus

  filenames <- list.files(pattern = ".txt", ignore.case = TRUE, recursive = TRUE) # List of all txt files in PATH (including subfolders)
  news_raw <- data.frame() # Empty data frame, the target
  j <- 0

  for(i in filenames) {
    news_temp <- readLines(i, encoding = "UTF-8")
    news_temp <- gsub("AUTOR:.*", " ", news_temp) # Here certain document markers are removed, exploiting the fact that they are saved as individual lines
    news_temp <- gsub("RUBRIK:.*", " ", news_temp)
    # news_temp <- gsub("LÄNGE:.*", " ", news_temp)
    news_temp <- gsub("SPRACHE: GERMAN", " ", news_temp)
    news_temp <- gsub("Copyright [0-9]{4}.*", " ", news_temp)
    news_temp <- paste(news_temp, collapse=" ") # Collapsing individual lines to a common string
    news_raw <- paste(news_raw, news_temp) # Paste string from file i to the news-raw dataframe
    rm(news_temp)
    j <- j + 1 # Counter
    print(paste("Read file: ", j, "/", length(filenames), sep = "")) # Progress report
  }

  rm(filenames, i, j)


  # Create article-based data frame
  #--------------------------------

  # Splitting into individual articles (based on LexNex "Dokument" markup pattern)
  rawdocs <- strsplit(news_raw, split = "Dokument [0-9]+ von [0-9]+", fixed = FALSE)

  rm(news_raw)

  # Turn this into a data frame with one oberservation per article
  articles <- as.data.frame(rawdocs, row.names = NULL)
  articles <- as.data.frame(articles[-1, ]) # Removing the faulty first line 
  names(articles)[1] <- paste("raw_text") # Renaming variable

  # Extracting some descriptive variables from raw article texts
  ##############################################################

  # Article length

  lengthhelp <- as.data.frame(str_extract(articles$raw_text, "LÄNGE: [0-9]+ Wörter")) # Extracting the article word count as deliverred by LexNex
  articles$raw_text <- str_replace(articles$raw_text, "LÄNGE: [0-9]+ Wörter", " ") # Removing the parsed information from the raw text

  names(lengthhelp)[1] <- paste("raw_length") # Renaming variable

  length <- as.data.frame(str_extract(lengthhelp$raw_length, "[0-9]+"))
  names(length)[1] <- paste("article_length") # Renaming variable

  rm(lengthhelp)

  # Publication date

#   pubdatehelp <- as.data.frame(str_extract(articles$raw_text, "(January|February|March|April|May|June|July|August|September|October|November|December) [0-9]+, [0-9]{4}"))
#   articles$raw_text <- str_replace(articles$raw_text, "(January|February|March|April|May|June|July|August|September|October|November|December) [0-9]+, [0-9]{4}, (Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)", " ")
#   articles$raw_text <- str_replace(articles$raw_text, "(January|February|March|April|May|June|July|August|September|October|November|December) [0-9]+, [0-9]{4} (Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)", " ") # Sometimes, the weekday was not separated by a comma
#   articles$raw_text <- str_replace(articles$raw_text, "(January|February|March|April|May|June|July|August|September|October|November|December) [0-9]+, [0-9]{4}", " ") # Sometimes, the weekday is not given at all

  pubdatehelp <- as.data.frame(str_extract(articles$raw_text, "[0-9]{1,2}\\. (Januar|Februar|März|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember) [0-9]{4}"))
  articles$raw_text <- str_replace(articles$raw_text, "[0-9]{1,2}\\. (Januar|Februar|März|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember) [0-9]{4}", " ")
#   articles$raw_text <- str_replace(articles$raw_text, "(January|February|March|April|May|June|July|August|September|October|November|December) [0-9]+, [0-9]{4} (Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)", " ") # Sometimes, the weekday was not separated by a comma
#   articles$raw_text <- str_replace(articles$raw_text, "(January|February|March|April|May|June|July|August|September|October|November|December) [0-9]+, [0-9]{4}", " ") # Sometimes, the weekday is not given at all
  
  
  names(pubdatehelp)[1] <- paste("pub_date") # Renaming variable
  pubdatehelp <- as.data.frame(str_replace(pubdatehelp$pub_date, ",", ""))
  names(pubdatehelp)[1] <- paste("pub_date") # Renaming variable

  pubdatehelp$pub_date <- sub(" Januar ", "01.", pubdatehelp$pub_date, fixed = TRUE)
  pubdatehelp$pub_date <- sub(" Februar ", "02.", pubdatehelp$pub_date, fixed = TRUE)
  pubdatehelp$pub_date <- sub(" März ", "03.", pubdatehelp$pub_date, fixed = TRUE)
  pubdatehelp$pub_date <- sub(" April ", "04.", pubdatehelp$pub_date, fixed = TRUE)
  pubdatehelp$pub_date <- sub(" Mai ", "05.", pubdatehelp$pub_date, fixed = TRUE)
  pubdatehelp$pub_date <- sub(" Juni ", "06.", pubdatehelp$pub_date, fixed = TRUE)
  pubdatehelp$pub_date <- sub(" Juli ", "07.", pubdatehelp$pub_date, fixed = TRUE)
  pubdatehelp$pub_date <- sub(" August ", "08.", pubdatehelp$pub_date, fixed = TRUE)
  pubdatehelp$pub_date <- sub(" September ", "09.", pubdatehelp$pub_date, fixed = TRUE)
  pubdatehelp$pub_date <- sub(" Oktober ", "10.", pubdatehelp$pub_date, fixed = TRUE)
  pubdatehelp$pub_date <- sub(" November ", "11.", pubdatehelp$pub_date, fixed = TRUE)
  pubdatehelp$pub_date <- sub(" Dezember ", "12.", pubdatehelp$pub_date, fixed = TRUE)
  
  pubdatehelp$pub_date <- as.Date(pubdatehelp$pub_date, "%d.%m.%Y")
  
  # Sys.setlocale("LC_TIME", "English")
  # pubdate <- as.data.frame(as.Date(pubdatehelp$pub_date, "%d.%B.%Y"))
  pubdate <- pubdatehelp
  names(pubdate)[1] <- paste("pub_date") # Renaming variable

  rm(pubdatehelp)

  # Page number
# 
#   pagehelp <- as.data.frame(str_extract(articles$raw_text, "SECTION:.*(Pg\\.|Page) ([0-9]+|I|II|III|IV|V|VI|VII|VIII|IX|X)")) # Sometimes page numbers are given in roman numbers
#   articles$raw_text <- str_replace(articles$raw_text, "SECTION:.*?(Pg\\.|Page) ([0-9]+|I|II|III|IV|V|VI|VII|VIII|IX|X)", " ") # - here is the error - regular expressions are greedy and some article have a 'page' in the end
# 
#   names(pagehelp)[1] <- paste("page") # Renaming variable

#   pagehelp$page <- str_replace(pagehelp$page, "SECTION:.*(Pg\\.|Page)", "") # Removes everything until the page number comes 
#   page <- str_extract(pagehelp$page, "[0-9]+|I|II|III|IV|V|VI|VII|VIII|IX|X") # Extracts the first arabic or roman number that appears
# 
#   ### !!! Pg. A4 ####
# 
#   rm(pagehelp)
# 
#   page <- str_replace(page, "VIII", "8")
#   page <- str_replace(page, "VII", "7")
#   page <- str_replace(page, "VI", "6")
#   page <- str_replace(page, "IV", "4")
#   page <- str_replace(page, "IX", "9")
#   page <- str_replace(page, "III", "3")
#   page <- str_replace(page, "II", "2")
#   page <- str_replace(page, "I", "1")
#   page <- str_replace(page, "V", "5")
#   page <- str_replace(page, "X", "10")
# 
#   page[is.na(page)] <- 0 # Sets all missing page numbers to zero
# 
#   page  <- as.data.frame(page)
#   names(page)[1] <- paste("page") # Renaming variable
  
  # Newspaper source

  newspaper <- as.data.frame(str_extract(articles$raw_text, "^[ ]+[A-Za-z]+ [A-Za-z]+")) # First two alpha terms after any number of whitespaces in the beginning of the string
  articles$raw_text <- sub("^[ ]+[A-Za-z]+ [A-Za-z]+", "", articles$raw_text, fixed = FALSE)
  names(newspaper)[1] <- paste("newspaper") # Renaming variable
  newspaper <- as.data.frame(str_trim(newspaper$newspaper, side = "both")) # Trims the unnecessary white spaces left and right of the source name
  names(newspaper)[1] <- paste("newspaper") # Renaming variable
  

  # Cleaning data frame 
  #--------------------

  # Combining the variables

  corpus <- as.data.frame(c(newspaper, pubdate, length, articles))
  rm(newspaper, pubdate, length, articles)

  # Change some variable classes
  
  corpus$article_length <- as.numeric(as.character(corpus$article_length)) # was a factor before
  # corpus$page <- as.numeric(as.character(corpus$page)) # was a factor before
  
  
  # Removing double entries from LexNex

  corpus <- unique(corpus)

  # Relabelling newspaper sources - ADAPT TO YOUR APPLICATION

  # unique(corpus$newspaper)
  # corpus <- corpus[!is.na(corpus$newspaper), ] # Drop observations for which no newspaper source is coded (Guardian.com)


  # Cleaning the raw texts
  #-----------------------


  # Removing the source markers from raw text

#   corpus$raw_text <- str_replace_all(corpus$raw_text, "(Financial Times \\(London,England\\)|The New York Times|The Straits Times \\(Singapore\\))", "")
#   corpus$raw_text <- str_replace_all(corpus$raw_text, "Financial Times \\(London, England\\)", "") # From FT
#   corpus$raw_text <- str_replace_all(corpus$raw_text, "Financial Times \\(London\\) \\(London,England\\)", "") # From FT
#   corpus$raw_text <- str_replace_all(corpus$raw_text, "The Guardian \\(London\\)", "")
#   corpus$raw_text <- str_replace_all(corpus$raw_text, "Guardian Unlimited", "")
#   corpus$raw_text <- str_replace_all(corpus$raw_text, "The Guardian - Final Edition", "")
#   corpus$raw_text <- str_replace(corpus$raw_text, "- Final Edition", "") # Guardian leftover
#   corpus$raw_text <- str_replace(corpus$raw_text, "The Washington Post( )+(,){0,1}( [A-Za-z],){0,1}( )+(Final|Suburban|Regional|Met 2|Every) Edition", "")
 
  # Removing white spaces

  corpus$raw_text <- str_trim(corpus$raw_text)

  # Some manual cleaning based on observations of regularties in articles - ADAPT TO YOUR CORPUS

  ## Newspaper lines here
  
  corpus$raw_text <- gsub("/", " ", corpus$raw_text) # Removes any slash from the text and replaces it with a space
  corpus$raw_text <- gsub("\\", "", corpus$raw_text, fixed = TRUE) # Should remove any backslash from the text - WORKING?
  corpus$raw_text <- gsub("\t", " ", corpus$raw_text) # Should remove any tabulator in the raw text and replace it with a simple space
  corpus$raw_text <- gsub(";", ",", corpus$raw_text) # Replaces any semicolon with a comma (so as not to interfere with the separators when exporting)

  corpus$raw_text <- str_trim(corpus$raw_text, side = "both") # Removes whitespaces left and right

  # Extracting the article headline

  corpus$headline <- str_replace(corpus$raw_text, "[ ]{2,}.*", "") # Deletes every character after at least two whitspace occurred (i.e. keeps the first sentence in raw text)

  # Removing double entries from LexNex
  
  corpus <- unique(corpus)
  
  # Ensuring that the function outputs the corpus data frame

  corpus

}


# Import articles downloaded from Lexis Nexis
# HEADLINE(Wulff)
#############################################

corpus <- LexClean(paste(getwd(),"/RawTexts", sep=""))

corpus$headline <- NULL
corpus <- corpus[order(corpus$pub_date), ]

# Export corpus
###############

setwd(sub("/RawTexts", "", getwd()))
save(corpus, file = "WulffCorpus.Rdata")
