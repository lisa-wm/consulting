# ------------------------------------------------------------------------------
# TOPIC MODELING - KEYWORD-BASED
# ------------------------------------------------------------------------------

# IN: dfm object with original documents and topic modeling features
# OUT: corpus object annotated with topic labels + file with topic variables

# SET KEYWORDS -----------------------------------------------------------------

load_rdata_files(tweets_dfm_tm, folder = "2_code")

# Define keywords and number of by-terms (frequent co-occurrences of the 
# keyword, in addition to word-stem-based derivatives) to be retrieved

keywords <- c("corona", "klima", "europa", "wirtschaft")
n_byterms <- 10L

# CREATE FEATURE-CO-OCCURENCE MATRIX -------------------------------------------

tweets_fcm <- quanteda::fcm(tweets_dfm_tm, tri = FALSE)

# Check whether keywords are among the text features

stopifnot(keywords %in% quanteda::featnames(tweets_fcm))

# If not, try similar words and perhaps adjust keywords

quanteda::featnames(tweets_fcm)[startsWith(
  quanteda::featnames(tweets_fcm), 
  "foo")]

# Convert back to dfm for convenient feature selection and 
# select keywords only

tweets_fcm_dfm <- quanteda::as.dfm(tweets_fcm) %>% 
  quanteda::dfm_select(keywords)

keywords_byterms <- as.data.table(convert(tweets_fcm_dfm, to = "data.frame"))

# DETERMINE TOP CO-OCCURRING TERMS FOR KEYWORDS --------------------------------

# Order by-terms by co-occurrence

keywords_byterms[
  , c(keywords) := lapply(
    .SD, 
    function(i) doc_id[order(i, decreasing = TRUE)]), .SDcols = keywords
  ][, doc_id := NULL]

# REMOVE DUPLICATES ------------------------------------------------------------

# To make this faster, first prune data to maximum required length

keywords_byterms <- keywords_byterms[1:(ncol(keywords_byterms) * n_byterms)]

# Remove terms that co-occur with other keywords in higher frequencies to keep
# terms unique to topics

keywords_byterms_unique <- lapply(
  
  seq_along(keywords_byterms), 
  
  function(i) {
    
    duplicates <- find_duplicate_occurrence(keywords_byterms)
    keywords_byterms[, i, with = FALSE][!duplicates[, i]]
    
  }
)

# FIND DERIVATIVES OF KEYWORDS AND ADD TO LIST ---------------------------------

# Find similar words (containing a literal match of the keywords), along with 
# their frequency

keywords_derivatives <- lapply(
  
  seq_along(keywords),
  
  function(k) {
    
    derivatives <- unlist(stringr::str_extract_all(
      quanteda::featnames(tweets_fcm),
      paste0("(.?)*(", keywords[k], ")(.?)*")))
    
    freq_dt <- as.data.table(
      featfreq(tweets_fcm)[unlist(derivatives)],
      keep.rownames = TRUE)
    
    setnames(freq_dt, c("derivative", "freq"))
    setorder(freq_dt, -freq)
    
  })

names(keywords_derivatives) <- keywords

# Merge with co-occurrence list and prune to desired length

keywords_list <- lapply(
  seq_along(keywords_derivatives), 
  function(k) {
    c(
      keywords_derivatives[[k]]$derivative, 
      keywords_byterms[[k]][1:n_byterms])})

# FIND MATCHES BETWEEN DOCUMENTS AND KEYWORDS (OR ASSOCIATED TOP TERMS) --------

# For each keyword, find the documents matching the word itself or its by-terms

matches_per_keyword <- lapply(
  
  seq_along(keywords), 
  
  function(k) {
    
    keyword_features <- keywords_list[[k]]
    matches_k <- quanteda::dfm_match(
      tweets_dfm_tm,
      keyword_features)
    
  })

names(matches_per_keyword) <- keywords

# RETRIEVE POSITIONS IN TOPIC KEYWORD LISTS THAT ARE MATCHED -------------------

# Recall that by-terms are ordered by number of co-occurrences with the keyword, 
# so match positions indicate topical congruence 

topic_matches <- data.table(doc_id = docnames(tweets_dfm_tm))

topic_matches[
  
  # Create column for every topic
  
  , sapply(keywords, function(k) paste0("topic_", k)) 
  
  := lapply(
    
    seq_along(keywords),
    
    function(k) {
      
      this_dfm <- matches_per_keyword[[k]]
      
      # For each document-topic combination, extract binary match list
      
      matches_d <- lapply(
        doc_id,
        function (d) {
          this_row <- dfm_subset(this_dfm, docnames(this_dfm) == d)
          this_df <- convert(this_row, to = "data.frame")
          column_to_rownames(this_df, "doc_id")})
      
      # For each document-topic combination, retrieve positions of matches
      # This omits cases where words are mentioned twice in one tweet, which 
      # should be really rare and not worth the effort to incorporate
      
      matches_d_pos <- lapply(
        seq_along(matches_d), 
        function(m) which(matches_d[[m]] == 1))
      
      # In case of multiple matches, calculate score to balance position and 
      # number of matches
      # TODO find better heuristic
      
      lapply(
        seq_along(matches_d_pos), function(m) {
          ifelse(
            length(matches_d_pos[[m]]) <= 1,
            matches_d_pos[[m]],
            min(matches_d_pos[[m]]) / length(matches_d_pos[[m]]))})
      
      })]

# ASSIGN TOPIC LABELS ----------------------------------------------------------

# Convert topic columns from list to numeric

topic_cols <- paste0("topic_", keywords)
topic_matches[, c(topic_cols) := lapply(.SD, as.numeric), .SDcols = topic_cols]

# Assign topic labels, accounting for a number of cases

topic_matches[
  , topic_label := which.min(.SD),
  .SDcols = topic_cols,
  by = seq_len(nrow(topic_matches))
  ][, topic_name := keywords[topic_label]
    ][, doc_id := as.numeric(doc_id)]

# MAP TOPIC LABLES TO ORIGINAL DOCUMENTS ---------------------------------------

# Extract docvars as data.table for modification (more convenient than quanteda 
# implementation in data.frame format)

docvars_dt <- as.data.table(docvars(tweets_dfm_tm))

# Create ID for pseudo-documents equivalent to what quanteda assigns internally 
# when grouping dfm

docvars_dt[, doc_id := .I]

# Append topic labels

docvars_dt <- topic_matches[docvars_dt, on = "doc_id"]

# Insert modified docvars back into corpus

tweets_corpus_topics_keywordbased <- tweets_corpus
docvars(tweets_corpus_topics_keywordbased) <- as.data.frame(docvars_dt)

# Save

save_rdata_files(tweets_corpus_topics_keywordbased, "2_code")

# EXTRACT SOME DESCRIPTIVE STATISTICS ------------------------------------------

party_colors <- c(
  "deepskyblue",
  "chartreuse4",
  "black",
  "deeppink3",
  "darkgoldenrod1",
  "red"
)

# Tweets that could be assigned a topic label

docvars(tweets_corpus_topics_keywordbased) %>% 
  group_by(party, topic_name) %>%
  mutate(topic_name = ifelse(is.na(topic_name), "99_none", topic_name)) %>%
  ggplot(aes(x = topic_name)) + 
  geom_bar()

# Tweets per topic and party

docvars(tweets_corpus_topics_keywordbased) %>% 
  group_by(party, topic_name) %>% 
  mutate(topic_name = ifelse(is.na(topic_name), "99_none", topic_name)) %>%
  filter(topic_name != "99_none") %>% 
  ggplot(aes(x = topic_name, fill = party)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = party_colors)
