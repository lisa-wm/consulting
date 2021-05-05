# ------------------------------------------------------------------------------
# PREPARE DATA FOR TOPIC MODELING EXERCISE
# ------------------------------------------------------------------------------

# Source: https://www.kaggle.com/hsankesara/medium-articles?select=articles.csv

# READ DATA --------------------------------------------------------------------

article_data <- data.table::fread(
  here::here("5_seminar/Contents_Part_02/exercises", "articles.csv"), 
  encoding = "UTF-8", 
  sep = ",")

# PERFORM BASIC CLEANING -------------------------------------------------------

article_data[, aux := seq_len(.N), by = text]
article_data <- article_data[aux == 1L]
article_data[, aux := NULL]

article_data[
  , text := stringr::str_remove_all(text, "\\n")
  ][, author := stringr::str_remove_all(author, "\U0001f52e\U0001f528")
    ][, claps := as.numeric(stringr::str_remove_all(claps, "[:alpha:]"))
      ][, aux := seq_len(.N), by = author
        ][, aux_2 := stringr::str_remove_all(author, "[:space:]")
          ][, doc_id := sprintf("%s_%d", aux_2, aux)
            ][, `:=`(aux = NULL, aux_2 = NULL)]

# ADD SOME FICTIONAL META DATA -------------------------------------------------

set.seed(123L)

country <- sample(
  c("US", "UK", "China", "India", "France", "Germany"),
  nrow(article_data),
  replace = TRUE)

source <- sample(
  c("commercial", "private"),
  nrow(article_data),
  replace = TRUE)

article_data[, `:=`(country = ..country, source = ..source)]

data.table::setkey(article_data, "doc_id")

# CREATE CORPUS AND SAVE -------------------------------------------------------

article_corpus <- quanteda::corpus(
  article_data, 
  docid_field = "doc_id",
  text_field = "text")

saveRDS(
  article_corpus, 
  file = here::here("5_seminar/Contents_Part_02/exercises", "articles.RDS"))
