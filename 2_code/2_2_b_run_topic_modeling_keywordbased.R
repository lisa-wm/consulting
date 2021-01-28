# ------------------------------------------------------------------------------
# TOPIC MODELING - KEYWORD-BASED
# ------------------------------------------------------------------------------

# IN: dfm object with original documents and topic modeling features
# OUT: corpus object annotated with topic labels + file with topic variables

# SET KEYWORDS -----------------------------------------------------------------

load_rdata_files(tweets_dfm_tm, folder = "2_code")

# Define keywords and number of by-terms (frequent co-occurrences of the 
# keyword, in addition to word-stem-based derivatives) to be retrieved

keywords <- list(
  corona = c("corona", "pandemie", "virus", "krise"),
  klima = c("klima", "grün", "natur", "umwelt")
)

keywords_clean <- lapply(
  keywords,
  function(i) {
    SnowballC::wordStem(remove_umlauts(i), language = "de")
  }
)

n_byterms <- 10L

# TODO check out quantedas keyness statistic instead of fcm

# CREATE FEATURE-CO-OCCURENCE MATRIX -------------------------------------------

tweets_fcm <- quanteda::fcm(tweets_dfm_tm, tri = FALSE)

# Check whether keywords are among the text features

stopifnot(names(keywords_clean) %in% quanteda::featnames(tweets_fcm))

# If not, try similar words and perhaps adjust keywords

quanteda::featnames(tweets_fcm)[startsWith(
  quanteda::featnames(tweets_fcm), 
  "foo")]

# Convert back to dfm for convenient feature selection and 
# select keywords only

tweets_fcm_dfm <- quanteda::as.dfm(tweets_fcm) %>% 
  quanteda::dfm_select(unlist(keywords_clean)) 

# Remove keywords that were not found in data

keywords_clean_available <- lapply(
  seq_along(keywords_clean), 
  function(i) {
    intersect(keywords_clean[[i]], quanteda::featnames(tweets_fcm_dfm))
  }
)

names(keywords_clean_available) <- names(keywords_clean)

# FIND DERIVATIVES OF KEYWORDS AND ADD TO LIST ---------------------------------

# Find similar words (containing a literal match of the keywords), along with 
# their frequency

keywords_derivatives <- lapply(
  
  seq_along(keywords_clean_available),
  
  function(i) {
    
    freq_list <- lapply(
      
      seq_along(keywords_clean_available[[i]]),
      
      function(j) {
        
        derivatives <- unlist(stringr::str_extract_all(
          quanteda::featnames(tweets_fcm),
          sprintf("(.?)*(%s)(.?)*", keywords[[i]][j])))
        
        freq_dt <- as.data.table(
          quanteda::featfreq(tweets_fcm)[unlist(derivatives)],
          keep.rownames = TRUE)
        
        data.table::setnames(freq_dt, c("derivative", "freq"))
        data.table::setorder(freq_dt, -freq)
        
      }
      
    )
    
    freq_list <- do.call(rbind, freq_list)
    
  })

names(keywords_derivatives) <- names(keywords_clean_available)

# DETERMINE TOP CO-OCCURRING TERMS FOR KEYWORDS --------------------------------

# Find by-terms and order by co-occurrence

keywords_byterms <- lapply(
  
  seq_along(keywords_clean_available),
  
  function(i) {
    
    fcm_i <- quanteda::dfm_select(tweets_fcm_dfm, keywords_clean_available[[i]])
    dt_i <- convert_dfm_to_dt(fcm_i, key = NULL)
    
    dt_i[
      , keywords_clean_available[[i]] := lapply(
        .SD, 
        function(j) doc_id[order(j, decreasing = TRUE)]
        ), 
      .SDcols = keywords_clean_available[[i]]
      ][, doc_id := NULL]
    
    data.table::setcolorder(dt_i, keywords_clean_available[[i]])
    
    })

names(keywords_byterms) <- names(keywords_clean_available)

# REMOVE DUPLICATES ------------------------------------------------------------

# To make this faster, first prune data to maximum required length (corresponds 
# to worst case where all keywords and derivatives are duplicates across topics)

keywords_byterms_merged <- do.call(cbind, keywords_byterms)

n_derivatives <- sum(sapply(keywords_derivatives, nrow))
n_potential_duplicates <- ncol(keywords_byterms_merged) * n_byterms + 
  n_derivatives
keywords_byterms_short <- keywords_byterms_merged[1:n_potential_duplicates]
  
# Remove terms that co-occur with other keywords in higher frequencies to keep
# terms unique to topics

keywords_byterms_unique <- sapply(
  
  seq_along(keywords_byterms_short), 
  
  function(i) {
    
    duplicates <- find_duplicate_occurrence(keywords_byterms_short)
    keywords_byterms_short[, i, with = FALSE][!duplicates[, i]]
    
  },
  
  USE.NAMES = TRUE
  
)

keywords_byterms_unique <- sapply(
  
  names(keywords_clean_available),
  
  function(i) {
    
    unname(
      unlist(
        keywords_byterms_unique[grepl(i, names(keywords_byterms_unique))]))})

stopifnot(length(unlist(keywords_byterms_unique)) == 
            length(unique(unlist(keywords_byterms_unique))))

# Merge with co-occurrence list and prune to desired length

keywords_list <- lapply(
  
  seq_along(keywords_derivatives), 
  
  function(i) {
    
    derivatives <- keywords_derivatives[[i]]$derivative
    byterms <- unlist(keywords_byterms_unique[[i]])
    overlaps <- intersect(derivatives, byterms)
    remaining_byterms <- byterms
    
    if (length(overlaps)) {
      remaining_byterms <- 
        remaining_byterms[!(remaining_byterms %in% overlaps)]
    }
    
    list(
      keyword = names(keywords_derivatives)[i],
      derivatives = derivatives,
      byterms = unname(remaining_byterms[1:n_byterms]))

    })

names(keywords_list) <- names(keywords_clean_available)

# MATCH DOCUMENTS WITH KEYWORDS ------------------------------------------------

# Create dictionary objects with multiple levels (per keyword: the keyword
# itself, its derivatives, and its by-terms)

dict_keywords <- quanteda::dictionary(keywords_list)

matches_dfm <- quanteda::dfm_lookup(
  tweets_dfm_tm, 
  dict_keywords,
  levels = 1:3)

tweets_matches <- convert_dfm_to_dt(matches_dfm, key = "doc_id")

# ASSIGN TOPIC LABELS ----------------------------------------------------------

# Take multi-step approach: first, assign all documents that can be 
# unambiguously matched (because they only match one topic, or the level of one
# topic match is strictly higher than that of others); second, check for all
# ambiguous cases on which position their matches are in the keyword list
# (matches higher up obtain priority)

tweets_matches_none <- tweets_matches[
  , n_matches := sum(.SD),
  .SDcols = -c("doc_id"),
  by = seq_len(nrow(tweets_matches))
  ][n_matches == 0]

tweets_matches_any <- tweets_matches[!(doc_id %in% tweets_matches_none$doc_id)]

# FIXME find a goddam fucking way to rank this

# topic_ranks <- lapply(
#   
#   c("keyword", "derivatives", "byterms"),
#   
#   function(i) {
#     
#     cols_i <- names(tweets_matches_any)[which(
#       endsWith(names(tweets_matches_any), i) | 
#         names(tweets_matches_any) == "doc_id")]
#     
#     dt_i <- tweets_matches_any[, ..cols_i]
#     data.table::setnames(dt_i, c("doc_id", names(keywords_clean_available)))
#     
#     foo <- t(apply(dt_i[, .(corona, klima)], 1, frank))
#     
#     dt_i[, c("corona", "klima") := apply(dt_i[, .(corona, klima)], 1, frank)]
#     
#     dt_i[, rank := frank(.SD), .SDcols = names(keywords_clean_available), 
#          by = doc_id]
#     
#   }
#   
# )
# 
# 
# test <- lapply(
#   
#   seq_along(keywords_clean_available),
#   
#   function(i) {
#     
#     topic_i <- names(keywords_clean_available)[i]
#     cols_i <- names(tweets_matches_any)[which(
#       startsWith(names(tweets_matches_any), topic_i) | 
#         names(tweets_matches_any) == "doc_id")]
#     dt_i <- tweets_matches_any[
#       , ..cols_i
#       ][, sum_matches := sum(.SD),
#         .SDcols = -c("doc_id"), 
#         by = doc_id]
#     
#     list(
#       keyword = dt_i[, .(doc_id, get(sprintf("%s.keyword", topic_i)))],
#       derivatives = dt_i[, .(doc_id, get(sprintf("%s.derivatives", topic_i)))],
#       byterms = dt_i[, .(doc_id, get(sprintf("%s.byterms", topic_i)))],
#       sum_matches = dt_i[, .(doc_id, sum_matches)]
#     )
#     
#     })
# 
# names(test) <- names(keywords_clean_available)
# foo <- data.table::setDT(do.call(cbind, test))
# 
# foo_keywords <- data.table::setDT(do.call(rbind, test))
# 
# foo <- data.table(
#   id = rep(1, 6),
#   topic = c(rep(1, 3), rep(2, 3)),
#   type = rep(c("k", "d", "b"), 2),
#   matches = c(1, 0, 1, 0, 2, 1)
# )
# 
# foo <- tweets_matches_any[1:20]
# 
# t(apply(foo, 1, rank))
# 
# match_cols <- names(tweets_matches)[sapply(tweets_matches, is.numeric)]
# 
# tweets_matches[, `:=` (
#   n_matches = sum(.SD), 
#   n_topics_matched = sum(.SD > 0)),
#   .SDcols = match_cols,
#   by = seq_len(nrow(tweets_matches))]
# 
# tweets_matches_none <- tweets_matches[
#   n_matches == 0
#   ][, topic_label := NA
#     ][, .(doc_id, topic_label)]
# 
# tweets_matches_unambiguous <- tweets_matches[
#   n_matches > 0 & n_lists_matched == 1
#   ][, topic_label := which.max(.SD),
#     .SDcols = match_cols,
#     by = doc_id
#     ][, .(doc_id, topic_label)]
# 
# tweets_matches_ambiguous <- tweets_matches[
#   n_matches > 0 & n_lists_matched != 1
#   ][, foo := ifelse(n_matches %% n_lists_matches)]
# 
# # [, topic_label := ifelse(
# #       n_topics_matched > 1 & n_matches %% n_topics_matched > 0, 
# #       which.max(.SD), 0), 
# #       .SDcols = match_cols, 
# #       by = doc_id]
# 
# # RETRIEVE POSITIONS IN TOPIC KEYWORD LISTS THAT ARE MATCHED -------------------
# 
# # Recall that by-terms are ordered by number of co-occurrences with the keyword,
# # so match positions indicate topical congruence 
# 
# # TODO make this faster
# 
# topic_matches <- data.table::data.table(
#   doc_id = quanteda::docnames(tweets_dfm_tm))
# 
# topic_matches[
#   
#   # Create column for every topic
#   
#   , sapply(keywords, function(i) sprintf("topic_%d", i)) 
#   
#   := lapply(
#     
#     seq_along(keywords),
#     
#     function(i) {
#       
#       this_dfm <- matches_per_keyword[[i]]
#       
#       # For each document-topic combination, extract binary match list
#       
#       matches_d <- lapply(
#         doc_id,
#         function (j) {
#           this_row <- quanteda::dfm_subset(this_dfm, docnames(this_dfm) == j)
#           this_df <- quanteda::convert(this_row, to = "data.frame")
#           tibble::column_to_rownames(this_df, "doc_id")})
#       
#       # For each document-topic combination, retrieve positions of matches
#       # This omits cases where words are mentioned twice in one tweet, which 
#       # should be really rare and not worth the effort to incorporate
#       
#       matches_d_pos <- lapply(
#         seq_along(matches_d), 
#         function(j) which(matches_d[[j]] == 1))
#       
#       # In case of multiple matches, calculate score to balance position and 
#       # number of matches
#       # TODO find better heuristic
#       
#       lapply(
#         seq_along(matches_d_pos), function(j) {
#           ifelse(
#             length(matches_d_pos[[j]]) <= 1,
#             matches_d_pos[[j]],
#             min(matches_d_pos[[j]]) / length(matches_d_pos[[j]]))})
#       
#       })]
# 
# svDialogs::msg_box("done matching topics")
# 
# save_rdata_files(topic_matches, folder = "2_code/2_topic_extraction")
# 
# # ASSIGN TOPIC LABELS ----------------------------------------------------------
# 
# load_rdata_files(topic_matches, folder = "2_code/2_topic_extraction")
# 
# # Convert topic columns from list to numeric
# 
# topic_cols <- sprintf("topic_%d", keywords)
# topic_matches[, c(topic_cols) := lapply(.SD, as.numeric), .SDcols = topic_cols]
# 
# # Assign topic labels
# 
# topic_matches[
#   , topic_label := which.min(.SD),
#   .SDcols = topic_cols,
#   by = seq_len(nrow(topic_matches))
#   ][, topic_name := keywords[topic_label]]
# 
# # MAP TOPIC LABLES TO ORIGINAL DOCUMENTS ---------------------------------------
# 
# # Extract docvars as data.table for modification (more convenient than quanteda 
# # implementation in data.frame format)
# 
# docvars_dt <- as.data.table(quanteda::docvars(tweets_dfm_tm))
# 
# # Create ID equivalent to what quanteda assigns internally when creating dfm
# # TODO insert setkey statement
# 
# docvars_dt[, doc_id := quanteda::docid(tweets_dfm_tm)]
# 
# # Append topic labels
# 
# docvars_dt <- topic_matches[docvars_dt, on = "doc_id"]
# 
# # Insert modified docvars back into corpus
# 
# tweets_corpus_topics_keywordbased <- tweets_corpus
# quanteda::docvars(tweets_corpus_topics_keywordbased) <- 
#   as.data.frame(docvars_dt)
# 
# # Save
# 
# save_rdata_files(tweets_corpus_topics_keywordbased, "2_code")
# 
# # EXTRACT SOME DESCRIPTIVE STATISTICS ------------------------------------------
# 
# party_colors <- c(
#   "deepskyblue",
#   "chartreuse4",
#   "black",
#   "deeppink3",
#   "darkgoldenrod1",
#   "red"
# )
# 
# # Tweets that could be assigned a topic label
# 
# docvars(tweets_corpus_topics_keywordbased) %>% 
#   group_by(party, topic_name) %>%
#   mutate(topic_name = ifelse(is.na(topic_name), "99_none", topic_name)) %>%
#   ggplot(aes(x = topic_name)) + 
#   geom_bar()
# 
# # Tweets per topic and party
# 
# docvars(tweets_corpus_topics_keywordbased) %>% 
#   group_by(party, topic_name) %>% 
#   mutate(topic_name = ifelse(is.na(topic_name), "99_none", topic_name)) %>%
#   filter(topic_name != "99_none") %>% 
#   ggplot(aes(x = topic_name, fill = party)) +
#   geom_bar(position = "dodge") +
#   scale_fill_manual(values = party_colors)
