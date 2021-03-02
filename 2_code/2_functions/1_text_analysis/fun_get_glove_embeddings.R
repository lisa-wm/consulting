library(tidyverse)
library(checkmate)
library(backports)
library(data.table)
library(text2vec)
library(readr)

# Set working directory to our project root: setwd("./consulting")
# The code below is based on the original documentation: 
# http://text2vec.org/glove.html


##' Train GloVe-wordvectors
##' @param data_path path to data
##' @param bigram TRUE or FALSE. Unigram is used if FALSE (default)
##' @param dimension size of word embeddings. Default: 50
##' @param output name of output file. Default: "glove_embeddings_tweets"
get_glove_embeddings <- function(data_path = "./2_code/0_training_data/data-tweepy-df-subset-labeled-manually.csv", 
                          bigram = FALSE, 
                          dimension = 50,
                          output = "glove_embeddings_tweets"){
  
  data <- read_delim(data_path, ";", 
                       escape_double = FALSE, trim_ws = TRUE)
  
  # Tokenize tweets
  tokens2 <- space_tokenizer(data$text)
  
  tokens <- quanteda::tokens(data$text)
  tokens_list <- as.list(tokens)
  
  it <- itoken(tokens_list, progressbar = FALSE)
  
  # TF und DF per token 
  vocab <- create_vocabulary(it)
   
  vocab <- prune_vocabulary(vocab, term_count_min = 1L)
  
  vectorizer <- vocab_vectorizer(vocab)
  
  # Create co-occurrence matrix
  tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L) 
  
  # Fit GLoVe Model
  glove <- GlobalVectors$new(rank = dimension, x_max = 10)
  wv_main <- glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01, n_threads = 8)  
  
  wv_context <- glove$components
  
  word_vectors <-  wv_main + t(wv_context)
  
  file_name <- paste(output, "glove.txt", sep = "_")
  
  write.table(word_vectors, file_name, append = FALSE, sep = " ", dec = ".",
              row.names = TRUE, col.names = FALSE, quote = FALSE)
  
}

# create_glove_embeddings()
