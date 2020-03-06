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

# We'll extract just the fields we need here.
medline_list_to_df <- function(medline_list) {
  med_df <- data.frame("PMID" = character(), 
                       "TA" = character(),
                       "MH" = character(),
                       "DP" = character())
  for(i in 1:length(medline_list)) {
    pmid <-  medline_list[[i]][["PMID"]]
    ta <-  medline_list[[i]][["TA"]]
    mh <- medline_list[[i]][["MH"]]
    dp <- medline_list[[i]][["DP"]]
    # So we only get things with mesh terms filled in here.
    if(length(mh) == 1 & length(ta) == 1 & length(pmid) == 1 & length(dp) == 1) {
      tdf <- data.frame("PMID" = pmid,
                        "TA" = ta,
                        "MH" = mh,
                        "DP" = dp)
      med_df <- bind_rows(med_df, tdf)
    }
  }
  return(med_df)
}


jbc_niche_articles <- data.frame("PMID" = character(), 
                                 "TA" = character(),
                                 "MH" = character(),
                                 "DP" = character())


for(j in 1:nrow(niche_terms)) {
  
  
  cat("Running year ", years[year_index], " and term ", niche_terms$MH[j], "\n")
  
  res_ta <- entrez_search(term = paste0(niche_terms$MH[j],"[mesh] and ", years[year_index], "[pdat]"), 
                          db = "pubmed", retmax = 9e4, api_key = api_key,
                          use_history = TRUE)
  
  if(length(res_ta$ids) > 0) {
    
    # This function may be the one that is failing.
    # Since we're getting intermittent curl failures
    # We would like to just catch the error and retry the function. 
    r <- NULL
    attempt <- 0
    while( is.null(r) && attempt <= 4 ) {
      attempt <- attempt + 1
      try(
        res_medline_chr <- entrez_fetch(db="pubmed", web_history = res_ta$web_history, 
                                        rettype = "MEDLINE",
                                        api_key = api_key)
      )
    } 
    

    res_medline_list <- medline_parser(res_medline_chr)
    medline_df <- medline_list_to_df(medline_list = res_medline_list)
    write.csv(medline_df, paste0("data/jbc_niche", year_index, "_", j, ".csv"), row.names = FALSE)
    # jbc_niche_articles <- bind_rows(jbc_niche_articles, medline_df)
  }
  
  # Break just in case.
  Sys.sleep(0.2)
}
# write.csv(jbc_niche_articles, "results/jbc_niche_articles_2005_2019.csv", row.names = FALSE)
