#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

year_index <- as.numeric(args[1])
print(year_index)


options(stringsAsFactors = FALSE)
source("util/util.R")
source("util/eutilities.R")
source("util/.ncbi_api_key.R")
library(rentrez)
library(tidyverse)

niche_terms <- read.csv("results/jbc_characteristic_terms.csv")
years <- 2005:2019



# for(j in 1:nrow(niche_terms)) {
# Just running the terms that failed due to multiple subheadings.
for(i in c(7,30)) {
  
  cat("Running year ", years[year_index], " and term ", niche_terms$MH[j], "\n")
  
  # For terms with multiple subheadings we're going to split them into their individual subheadings
  query <- make_query(niche_terms$MH[j])
  
  # Execute search
  res_ta <- entrez_search(term = query, 
                          db = "pubmed", 
                          retmax = 9e4, 
                          api_key = api_key,
                          use_history = TRUE)
  
  # Ensure that we retrive all papers even if there is more than 10k results
  # This is a way of batching the results retrieval.
  retstarts <- 1e4*(1:ceiling(res_ta$count/1e4)-1)
  
  # Let's make this robust to intermittent connection issues.
  # By retrying four times if we get an error.
  res_df <- NULL
  attempt <- 0
  while(is.null(res_df) && attempt <= 4) {
    attempt <- attempt + 1
    try(
      res_df <- do.call(rbind, lapply(retstarts, function(retstart) {
        entrez_fetch(db="pubmed", 
                     web_history = res_ta$web_history, 
                     rettype = "MEDLINE",
                     retstart = retstart) %>%
          medline_parser() %>%
          select("PMID", "MH", "TA", "DP")
      }))
    )
  }
  # Output the results.
  write.csv(res_df, paste0("data/jbc_niche", year_index, "_", j, ".csv"), row.names = FALSE)
  
  # Break just in case.
  Sys.sleep(0.2)
}
