fake_dfm <- quanteda::dfm(quanteda::corpus(
  data.frame(
    doc_id = c(1L:2L),
    text = c(
      "This overly restrictive BOW assumption is such a shame",
      "BOW assumption sucks"
    )),
  docid_field = "doc_id",
  text_field = "text"))

quanteda::textplot_wordcloud(fake_dfm, min_count = 1L, color = "darkgray")
