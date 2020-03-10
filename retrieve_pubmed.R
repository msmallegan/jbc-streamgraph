




retrieve_pubmed <- function(year, niche_terms) {
  # for(i in 1:length(niche_terms)) {
  # Just running the terms that failed due to multiple subheadings.
  res <- data.frame("PMID" = character(),
                       "MH" = character(),
                       "TA" = character(),
                       "DP" = character(),
                       "term" = character(),
                       "year" = integer())
  for(i in c(7,30)) {
    Sys.sleep(round(rnorm(1, mean = 5,sd = 2)))
    cat("Running year ", year, " and term ", niche_terms[[i]], "\n")
    
    # For terms with multiple subheadings we're going to split them into their individual subheadings
    query <- make_query(niche_terms[[i]], year)
    
    cat("With query: \n", query, "\n\n")

    # Execute search
    cat("search\n\n")
    
    res_ta <- entrez_search(term = query,
                            db = "pubmed",
                            retmax = 9e4,
                            api_key = api_key,
                            use_history = TRUE)
    cat("searchdone\n\n")

    # Ensure that we retrive all papers even if there is more than 10k results
    # This is a way of batching the results retrieval.
    cat("ret\n\n")
    retstarts <- 1e4*(1:ceiling(res_ta$count/1e4)-1)

    # Let's make this robust to intermittent connection issues.
    # By retrying four times if we get an error.
    
    res_df <- NULL
    attempt <- 0
    while(is.null(res_df) && attempt <= 4) {
      cat("inloog\n\n")
      attempt <- attempt + 1
      cat("wowo\n\n")
      try(
        res_df <- do.call(rbind, lapply(retstarts, function(retstart) {
          Sys.sleep(round(rnorm(1, mean = 5,sd = 2)))
          entrez_fetch(db="pubmed",
                       web_history = res_ta$web_history,
                       rettype = "MEDLINE",
                       api_key = api_key,
                       retstart = retstart) %>%
            medline_parser() %>%
            select("PMID", "MH", "TA", "DP")
        }))
      )
    }
    res_df$term <- niche_terms[[i]]
    res_df$year <- year
    # Append to data frame
    res <- bind_rows(res, res_df)
  }
  # Output the results.
  return(res)
}
