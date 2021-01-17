# ------------------------------------------------------------------------------
# DICTIONARY EXPANSION BY TOPIC-SPECIFIC TERMS
# ------------------------------------------------------------------------------

# IN: topic-word distributions + publicly available sentiment dictionaries
# OUT: topic-specific dictionaries + emoji dictionary

# STANDARD SENTIMENT DICTIONARIES ----------------------------------------------

# German polarity clues

german_polarity_clues <- get_dict_gpc()

#  SentiWS

senti_ws <- get_dict_sentiws()

# Dictionary

dict_sentiments <- quanteda::dictionary(
  list(
    positive = unique(c(german_polarity_clues$positive, senti_ws$positive)),
    negative = unique(c(german_polarity_clues$negative, senti_ws$negative))),
  tolower = FALSE)

# EMOJI DICTIONARY -------------------------------------------------------------

emojis_ranking <- data.table::fread(
  here("2_code/0_external_data", "emojis-sentiment-ranking.csv"),
  drop = c(1, 9), 
  col.names = c(
    "unicode", 
    "occurrences", 
    "position", 
    "negative", 
    "neutral",
    "positive", 
    "name"))

# TODO Check whether this the right encoding

emojis_ranking[
  , principal_emotion := which.max(.SD),
  .SDcols = c("negative", "neutral", "positive"),
  by = seq_len(nrow(emojis_ranking))
  ][, polarity := data.table::fcase(
    principal_emotion == 1, "positive",
    principal_emotion == 2, "neutral",
    principal_emotion == 3, "negative")]

dict_emojis <- quanteda::dictionary(
  list(
    positive = emojis_ranking[polarity == "positive", .(unicode)],
    negative = emojis_ranking[polarity == "negative", .(unicode)]),
  tolower = FALSE)