words <- c("dog", "cat", "author", "writing", "corporate", "C-level", "profit")
topics <- seq_len(3L)

fictional_tw_distribution <- data.table::as.data.table(
  expand.grid(words, topics))

data.table::setnames(fictional_tw_distribution, c("word", "topic"))

fictional_tw_distribution[
  , tw_prob := c(
    0.2, 0.1, 0.75, 0.85, 0.2, 0.25, 0.15, 
    0.85, 0.95, 0.1, 0.15, 0.05, 0.15, 0.05,
    0.1, 0.15, 0.05, 0.1, 0.95, 0.75, 0.8)]

tw_heatmap <- ggplot2::ggplot(
  fictional_tw_distribution,
  ggplot2::aes(x = topic, y = word, fill = tw_prob)) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_gradient(
    low = "deepskyblue",
    high = "deepskyblue4",
    name = "topic-word probability",
    limits = c(0L, 1L)) +
  ggplot2::theme(text = ggplot2::element_text(size = 20L))

ggplot2::ggsave(
  here::here("5_seminar/Contents_Part_02/slides", "heatmap.png"),
  tw_heatmap)
