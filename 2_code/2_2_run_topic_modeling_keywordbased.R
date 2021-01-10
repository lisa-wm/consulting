# ------------------------------------------------------------------------------
# TOPIC MODELING - KEYWORD-BASED
# ------------------------------------------------------------------------------

# IN: single data file of cleaned tweets and meta data + list of keywords
# OUT: single data file of cleaned tweets and meta data with topic label + 
# file of topic-word distributions

# SET KEYWORDS -----------------------------------------------------------------

# Define keywords and number of by-terms to be retrieved

keywords <- c("corona", "klima")
n_byterms <- 50L

# CREATE FEATURE-CO-OCCURENCE MATRIX -------------------------------------------

# Create fcm of all features

tweets_dfm_tm_grouped_top <- quanteda::dfm_trim(
  tweets_dfm_tm_grouped, 
  min_term_freq = 1L)

tweets_fcm <- quanteda::fcm(tweets_dfm_tm_grouped_top, tri = FALSE)

# Check whether keywords are among the text features

stopifnot(keywords %in% quanteda::featnames(tweets_fcm))

# Find similar words (containing a literal match of the keywords)

keywords_derivatives <- lapply(
  seq_along(keywords), 
  function(k) {
    unlist(stringr::str_extract_all(
      quanteda::featnames(tweets_fcm),
      paste0("(.?)*(", keywords[k], ")(.?)*")))})

names(keywords_derivatives) <- keywords

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
    function(i) doc_id[order(i, decreasing = TRUE)]), .SDcols = keywords]

# Confine to desired number of by-terms
# TODO find way to make keyword lists topic-unique

keywords_byterms[, doc_id := NULL]
keywords_byterms <- keywords_byterms[1:n_byterms]

# FIND MATCHES BETWEEN DOCUMENTS AND KEYWORDS OR (ASSOCIATED TOP TERMS) --------

# For each keyword, find the documents matching the word itself or its by-terms

matches_per_keyword <- lapply(
  
  seq_along(keywords), 
  
  function(k) {
    
    keyword_features <- keywords_byterms[, get(keywords[k])]
    matches_k <- quanteda::dfm_match(
      tweets_dfm_tm_grouped_top,
      keyword_features)
    
  })

names(matches_per_keyword) <- keywords

# RETRIEVE POSITIONS IN TOPIC KEYWORD LISTS THAT ARE MATCHED -------------------

# Recall that by-terms are ordered by number of co-occurrences with the keyword, 
# so match positions indicate topical congruence 

topic_matches <- data.table(doc_id = docnames(tweets_dfm_tm_grouped_top))

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
  ][, topic_name := keywords[topic_label]]
