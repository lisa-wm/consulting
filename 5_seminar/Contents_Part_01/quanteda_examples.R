data <- data.table::data.table(
  doc_id = seq_len(3L),
  text = c(
    "Politics have no relation to morals.",
    "Politics is too serious a matter to be left to the politicians.", 
    "In politics stupidity is not a handicap."),
  author = c("Niccolo Machiavelli", "Charles de Gaulle", "Napoleon Bonaparte"),
  nationality = c("Italian", "French", "French"))

knitr::kable(data, "simple")

crp <- quanteda::corpus(
  as.data.frame(data),
  docid_field = "doc_id",
  text_field = "text")

tks <- quanteda::tokens(
  crp,
  remove_punct = TRUE)

tks_clean <- quanteda::tokens_remove(tks, quanteda::stopwords())

dfm <- quanteda::dfm(tks_clean)

fcm <- quanteda::fcm(dfm)

dict <- quanteda::dictionary(list(
  political = c("politics", "politicians"),
  critical = c("morals", "stupidity", "handicap")))

quanteda::dfm_lookup(dfm, dict)
