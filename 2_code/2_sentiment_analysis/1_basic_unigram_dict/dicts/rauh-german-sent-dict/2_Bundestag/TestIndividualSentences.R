# The combined dictionary (CUZ)
###############################

load("CombGermanSentimentDictionary.Rdata") 
load("CombGermanSentimentDictionary_Neg.Rdata") 


# The test text
###############

text <- "  wir wissen , dass die südsudanesische armee ein viel größeres problem ist "

# Marking directly negated sentiment features in the source text
#---------------------------------------------------------------

# Loop over negation dictionary, and replace instances in text
text2 <- text

for (i in 1:nrow(negation)){
  text2 <- gsub(negation$pattern[i], negation$replacement[i], text2, fixed = TRUE)
}

text2 <- gsub("(\\.|\\?|!|:|,|;|-)", "", text2, fixed = FALSE)
text2 <- gsub("  ", " ", text2, fixed = FALSE)
text2 <- gsub("([a-zöüäß1-9])( )([a-zöüäß1-9])", "\\1 | \\3", text2, fixed = FALSE)


sent.dictionary$scored <- grepl(text2, sent.dictionary$feature, fixed = FALSE)

results <- sent.dictionary[sent.dictionary$scored == TRUE, ]


