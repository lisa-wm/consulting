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

