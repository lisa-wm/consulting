# ------------------------------------------------------------------------------
# EXTRACTION OF WORD EMBEDDINGS
# ------------------------------------------------------------------------------

# IN: data with topic labels
# OUT: data with loadings on word embeddings

# FIXME doc2vec does not work

load_rdata_files(tweets_corpus_topics_unsupervised, folder = "2_code")

load_rdata_files(tweets_corpus_tagged, folder = "2_code/3_sentiment_analysis")

tweets_important_words <- tweets_corpus_tagged[
  pos %in% c("VERB", "NOUN", "ADV", "ADJ", "PROPN")
  ][nchar(token) > 2
    ][, aux := "text"]

tweets_important_words <- data.table::dcast(
  tweets_important_words,
  doc_id ~ aux,
  value.var = "token",
  fun.aggregate = paste, 
  collapse = " ")

stopwords_sa <- make_stopwords_sa()
stopwords_sa <- stopwords_sa[nchar(stopwords_sa) > 2L]

tweets_important_words[
  , text := SnowballC::wordStem(tolower(text), language = "de")
  ][, text := stringr::str_squish(unlist(stringr::str_remove_all(
    text, 
    regex(str_c("\\b", stopwords_sa, "\\b", collapse = '|')))))]

tweets_topic_labels <- data.table(
  doc_id = quanteda::docid(tweets_corpus_topics_unsupervised),
  topic_label = tweets_corpus_topics_unsupervised$topic_label
)

tweets_dense <- tweets_topic_labels[
  tweets_important_words, on = "doc_id"
  ][, aux := "text_topic"]

tweets_dense_topics <- data.table::dcast(
  tweets_dense,
  topic_label ~ aux,
  value.var = "text",
  fun.aggregate = paste, 
  collapse = " ")

nchar(tweets_dense_topics$text_topic) # fuck

save_rdata_files(
  tweets_dense_topics, 
  folder = "2_code/3_sentiment_analysis")

# FIXME does not work, if text length exceeds 5000 or so words, function will
# simply package the texts into smaller documents (w/o warning......) which make
# no sense

# source code https://github.com/bnosac/doc2vec/blob/master/src/rcpp_doc2vec.cpp

if (FALSE) {
  
  foo <- copy(tweets_dense_topics)[
    , text_topic := substr(text_topic, 1L, 1000L)
  ]
  
  tweets_dense_topics_df <- as.data.frame(foo) %>% 
    rename("doc_id" = topic_label, "text" = text_topic)
  
  model <- doc2vec::paragraph2vec(
    tweets_dense_topics_df, 
    type = "PV-DBOW", 
    dim = 10L, 
    iter = 1L,
    sample = 0.0001, 
    min_count = 1L)
  
  embedding_docs <- as.matrix(model, which = "docs")
  embedding_words <- as.matrix(model, which = "words")
  
  head(embedding_docs)
  vocab <- summary(model, type = "vocabulary", which = "docs") 
  
}

# Glove implementation Asmik

if (FALSE) {
  
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
    tokens <- space_tokenizer(data$full_text)
    
    it <- itoken(tokens, progressbar = FALSE)
    
    # TF und DF per token 
    vocab <- create_vocabulary(it)
    
    vocab <- prune_vocabulary(vocab, term_count_min = 1L)
    
    vectorizer <- vocab_vectorizer(vocab)
    
    # Create co-occurrence matrix
    tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L) 
    
    # Fit GLoVe Model
    glove <- GlobalVectors$new(rank = dimension, x_max = 10)
    wv_main <- glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01, 
                                   n_threads = 8)  
    
    wv_context <- glove$components
    
    word_vectors <-  wv_main + t(wv_context)
    
    file_name <- paste(output, "glove.txt", sep = "_")
    
    write.table(word_vectors, file_name, append = FALSE, sep = " ", dec = ".",
                row.names = TRUE, col.names = FALSE, quote = FALSE)
    
  }
  
  # create_glove_embeddings()
  
}