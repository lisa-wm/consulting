# ------------------------------------------------------------------------------
# TOPIC MODELING - KEYWORD-BASED
# ------------------------------------------------------------------------------

# IN: single data file of cleaned tweets and meta data + list of keywords
# OUT: single data file of cleaned tweets and meta data with topic label + 
# file of topic-word distributions

# CREATE FEATURE-CO-OCCURENCE MATRIX -------------------------------------------

tweets_dfm_tm_grouped_top <- quanteda::dfm_trim(
  tweets_dfm_tm_grouped,
  min_termfreq = 400L)
nfeat(tweets_dfm_tm_grouped_top)

tweets_fcm <- as.data.table(
  as.matrix(quanteda::fcm(tweets_dfm_tm_grouped_top, tri = FALSE)))

# DETERMINE TOP CO-OCCURRING TERMS FOR KEYWORDS --------------------------------

# Define keywords and number of by-terms to be retrieved

keywords <- c("corona", "kind")
n_byterms <- 10L

# Create FCM subset for keywords

terms <- copy(names(tweets_fcm))
tweets_fcm[, terms := ..terms]
keywords_byterms <- tweets_fcm[, c("terms", ..keywords)]

# Order by-terms by co-occurrence

keywords_byterms[
  , c(keywords) := lapply(
    .SD, 
    function(i) terms[order(i, decreasing = TRUE)]), .SDcols = keywords]

# Confine to desired number of by-terms

keywords_byterms[, terms := NULL]
keywords_byterms <- keywords_byterms[1:n_byterms]

# FIND TWEETS MENTIONING KEYWORDS OR ASSOCIATED TOP TERMS ----------------------

keywords_byterms <- rbind(keywords_byterms, list("kakadu", "kaffee"))

fake_texts <- data.table(
  full_text = c(
    "hallo frau coronaz wie stehts um den kaffees",
    "der kakadu schaut dir beim kacken zu",
    "pudel gedudel"),
  doc_id = 1:3)

fake_corpus <- quanteda::corpus(
  fake_texts, 
  docid_field = "doc_id", 
  text_field = "full_text")

quickie <- as.data.table(
  quanteda::kwic(
    fake_corpus,
    pattern = paste0(keywords_byterms$corona, "*"),
    valuetype = "glob"))
quickie <- quickie[, .(docname, keyword, pattern)][, topic_label := 1]

quickie_2 <- as.data.table(
  quanteda::kwic(
    fake_corpus,
    pattern = paste0(keywords_byterms$kind, "*"),
    valuetype = "glob"))
quickie_2 <- quickie_2[, .(docname, keyword, pattern)][, topic_label := 2]


quickie_notsoquick <- bind_rows(quickie, quickie_2)

quickie_notsoquick[, pattern := as.character(str_remove_all(pattern, "[^a-z]"))]

quickie_notsoquick[, franktherank := lapply(
  seq_len(nrow(quickie_notsoquick)),
  function(i) {
    label_nr <- topic_label#[i]
    label_nr
    # this_col <- names(keywords_byterms)[label_nr]
    # wordi <- pattern[i]
    # pos <- which(keywords_byterms[, get(this_col)] == wordi)
    # pos - n_byterms * ((pos - 1) %/% n_byterms)
  })]

foo - n_byterms * (foo %/% n_byterms)

topic_assignments <- quickie_notsoquick[
  , .(franktherank = list(unique(topic_label))), by = docname]
quickie_quicker <- topic_assignments[quickie_notsoquick, on = "docname"]

quickie_notsoquick[
  , rankthetank := max(which(
    keywords_byterms == as.character(str_remove_all(.SD, "[^a-z]")))) %/% n_byterms, 
  by = seq_len(nrow(quickie_notsoquick)), .SDcols = pattern]
