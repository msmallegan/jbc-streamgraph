---
title: "Define JBC Terms"
output:
  html_document:
    keep_md: yes
editor_options: 
  chunk_output_type: console
  
---



## Retrieve PubMed MeSH terms for the year 2015

We may trim down the time window on either end, but for now we will be retrieving data for the time range 2005-2019. Last analysis we used the time window 2003-2013 and used the year 2013 to define the JBC niche terms. Here 2015 sits squarely in the limits of our analysis, but is recent enough to represent JBC's current profile.

#### Retrieve list of biological science journals


```r
query <- '("biological science disciplines"[MeSH Major Topic]) AND "periodical"[Publication Type] AND (ncbijournals[All Fields] AND English[lang])'
query_year <- 2015
```



```r
# Let's get the list of jourmals from the NLM database
jrnls <- esearch(query, db = "nlmcatalog",retmax = 10000)
jrnls <- esummary(jrnls, db = "nlmcatalog")
jrnls <- content(jrnls, as = "parsed")


# Extract journal metadata into dataframe
jids <- do.call(rbind, sapply(names(sapply(jrnls,names)), function(x) {
    jname <- x
    j <- jrnls[[x]]
    
    if("ISSNList.ISSNInfo.issn" %in% names(j)) { issn <- j$ISSNList.ISSNInfo.issn }
    else{issn <- NA}
    
    if("TitleMainList.TitleMain.SortTitle" %in% names(j)) { title <- j$TitleMainList.TitleMain.SortTitle}
    else{title <- NA}
    
    if("MedlineTA" %in% names(j)) { ta <- j$MedlineTA }
    else{ta <- NA}
    
    data.frame("uid" = x,"issn" = issn, "title" = title, "ta" = ta, stringsAsFactors = F)
    },
    simplify = F))

write.csv(jids, "results/NLM_Identifiers_for_Journals_in_biological_science_disciplines.csv", row.names = F)
# Cleanup
rm(jrnls, query)
```

#### Retreive list of article IDs for these journals


```r
# To learn about the search fields in pubmed: http://libguides.utoledo.edu/searchingpubmed/tags
tas <- jids[which(!is.na(jids$ta)), "ta"]

# Let's add some brittle, but useful caching...
# Be careful!
article_id_file <- paste0("results/pubmed_article_ids_", query_year, ".csv")
if(!file.exists(article_id_file)) {
  
  
  article_ids <- data.frame("ta" = character(0),"search_year" = numeric(0),
                            "article_count" = numeric(0), "uids" = character(0))
  
  # Should take on the order to five minutes. 
  for(ta in tas) {
    
    ta_res <- entrez_search(term = paste0(ta, "[TA] and ", query_year, "[pdat]"), 
                            db = "pubmed", retmax = 9e4, api_key = api_key)
    uids <- ta_res$ids
    uids_chr <- paste(uids,collapse = ";")
    
    temp <- data.frame("ta" = ta, "search_year" = query_year, 
                       "article_count" = length(uids), "uids" = uids_chr)
    article_ids <- rbind(article_ids, temp)
    
    # With the API key we can make up to 10 queries per second.
    # Otherwise it's just three queries per second
    Sys.sleep(time = 0.105)
  }
  write.csv(article_ids, article_id_file, row.names = F)
  
} else {
  article_ids <- read.csv(article_id_file)
}
```

#### Retrieve MeSH terms for these articles


```r
article_ids$uids <- as.character(article_ids$uids)
article_ids <- article_ids[which(article_ids$uids != ""),]

pmids <- paste(article_ids$uids, collapse=";")
pmids <- unlist(strsplit(pmids,";"))

# Since this process takes a long time,
# let's look at the previously retreived data and
# see if we can grab some of these article IDs from there

# ex <- fread("data/res/res10.csv", select = c("PMID","TA","MH"), sep = ",")
# fl <- list.files("data/res", full.names = T)
# # This will read in all of the previous results
# start_time <- Sys.time()
# res <- do.call(rbind, lapply(fl, fread, sep=",", select = c("PMID", "TA", "MH")))
# end_time <- Sys.time()
# 
# end_time - start_time


# Let's see which pmids are in our list
# length(which(res$PMID %in% pmids))
# Let's just save this for now
# save(res, file = "results/previous_pubmed_results.RData")


# Whoops, there's just a few because it's just the JBC niche subset.
# Okay, well I guess I'm going to have to fetch all of 2013. 

# test <- pmids[sample(10)]
# efetch_res <- efetch_batch(test)
# upload <- entrez_post(db="pubmed", id=pmids[1:50])
# upload
# ta <- "Nat Commun"
# ta2 <- "J Biol Chem"



concat_query <- function(journal_list, query_year) {
  if(length(journal_list) < 3) {
    stop("Journal list must be at least length three.")
  }
  qp1 <- "("
  qp2 <- "[TA] or "
  qp3 <- "[TA]) and "
  query_close <- paste0(query_year, "[pdat]")
  query <- ""
  for (i in 1:length(journal_list)) {
    if(i == 1) {
      query <- paste0(query, qp1, journal_list[i], qp2)
    } else if (i < length(journal_list)) {
      query <- paste0(query, journal_list[i], qp2)
    } else if (i == length(journal_list)) {
      query <- paste0(query, journal_list[i], qp3, query_close)
    }
  }
  return(query)
}


# We'll extract just the fields we need here.
medline_list_to_df <- function(medline_list) {
  med_df <- data.frame("PMID" = character(), 
                       "TA" = character(),
                       "MH" = character())
  for(i in 1:length(medline_list)) {
    pmid <-  medline_list[[i]][["PMID"]]
    ta <-  medline_list[[i]][["TA"]]
    mh <- medline_list[[i]][["MH"]]
    
    # So we only get things with mesh terms filled in here.
    if(length(mh) == 1 & length(ta) == 1 && length(pmid) == 1) {
          tdf <- data.frame("PMID" = pmid,
                      "TA" = ta,
                     "MH" = mh)
      med_df <- bind_rows(med_df, tdf)
    }
  }
  return(med_df)
}



# Okay, we've got a new approach this time so it doesn't take days to download the 
# data. This is awesome. Basically we'll concatenate search queries for a bunch
# of journals at once. Then we can use the web_history caching mechanism to 
# make sure our queries aren't too large. Baller.
# Careful with caching
article_mesh_file <- paste0("results/pubmed_article_mesh_", query_year, ".csv")
if(!file.exists(article_mesh_file)) {
  nchunks <- ceiling(nrow(article_ids) / 100)
  article_ids$ta <- as.character(article_ids$ta)
  
  article_mesh <- data.frame("PMID" = character(), 
                             "TA" = character(),
                             "MH" = character())
  
  
  for(i in 1:nchunks) {
    
    start <- 1 + ((i-1)*100)
    end <-  i*100
    
    cat("Running chunk ", i, "items ", start, " through ", end, "\n")
    
    journals <- article_ids$ta[start:end]
    journals <- journals[!is.na(journals)]
    
    # make query
    full_query <- concat_query(article_ids$ta[start:end], query_year = query_year)
    
    # These queries almost certainly won't go beyond the 10 queries per minute
    # So no need to put a brake on them
    res_ta <- entrez_search(term = full_query, 
                            db = "pubmed", retmax = 9e4, api_key = api_key,
                            use_history = TRUE)
    
    res_medline_chr <- entrez_fetch(db="pubmed", web_history = res_ta$web_history, rettype = "MEDLINE")
    res_medline_list <- medline_parser(res_medline_chr)
    medline_df <- medline_list_to_df(medline_list = res_medline_list)
    
    article_mesh <- bind_rows(article_mesh, medline_df)
  }
  
  write.csv(article_mesh, article_mesh_file, row.names = FALSE)
} else {
  article_mesh <- read.csv(article_mesh_file)
}
```



```r
# Some of the journals are missing. I wonder which ones and why.. damn I wish this just worked
missing_journals <- article_ids[which(!(article_ids$ta %in% unique(article_mesh$TA))), ]
fq <- concat_query(missing_journals$ta[101:200], query_year = query_year)
res_ta <- entrez_search(term = fq, 
                            db = "pubmed", retmax = 9e4, api_key = api_key,
                            use_history = TRUE)
res_medline_chr <- entrez_fetch(db="pubmed", web_history = res_ta$web_history, rettype = "MEDLINE")
res_medline_list <- medline_parser(res_medline_chr)
medline_df <- medline_list_to_df(medline_list = res_medline_list)
    
# Hmm, the first 100 generated 3 more hits. -- Maybe they really are missing?
length(unique(medline_df$TA))
```

```
## [1] 22
```

```r
# Otherwise the issue is with the way the queries are constructed -- and that would be a bummer. 
# I'm going to continue on for now. 
```




#### Summarize MeSH terms per journal

Also we'll filter terms that don't appear very often < 20 times in the dataset out of `nrow(article_mesh)` papers and `length(unique(article_mesh$PMID))` journals in the dataset. 


```r
length(unique(article_mesh$PMID))
```

```
## [1] 67841
```

```r
length(unique(article_mesh$TA))
```

```
## [1] 550
```

```r
article_mesh_long <- article_mesh %>% 
  separate_rows(MH, sep = ";") 

journal_mesh <- article_mesh_long %>%
  group_by(TA, MH) %>%
  summarize(count = n()) 

# We don't need to keep low frequency terms. Let's sum up the total occurence count of each term
# and remove the bottom.
term_freq <- article_mesh_long %>% group_by(MH) %>%
  summarize(count = n())

# Let's remove all of the terms that occur less than 20 times. This may be a parameter you would
# want to adjust. You may get slightly different results here.
hist(term_freq$count, breaks = 100)
```

![](01_define_jbc_niche_terms_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
summary(term_freq$count)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##     1.00     1.00     1.00     5.01     2.00 34457.00
```

```r
term_freq <- term_freq %>% filter(count > 20)

# This takes kind of a while and could be optimized with some solution from data.table (if you're 
# using R).
journal_mesh <- journal_mesh %>% filter(MH %in% term_freq$MH)
length(unique(journal_mesh$TA))
```

```
## [1] 550
```

```r
# Okay, this filtering didn't get rid of any journals, which makes me think
# it's an issue with the querying process. Bummer.
```

#### Calculate term frequency - inverse document frequency



```r
# Calculate TF-IDF

# Term frequency: number of times the term t appears in a doc / total number of terms in the document
# Let's first calculate the document length
# This is the number of mesh terms per journal. We could alternately do this based on the number
# Of papers per journal. Since each paper has multiple mesh terms, this will change things a bit.
# For now let's just do it based on total number of mesh terms in the journal in this year.
doc_len <- journal_mesh %>% group_by(TA) %>% summarize(doc_length = sum(count))

# IDF(t) = log_e(Total number of documents / Number of documents with term t in it).
total_num_docs <- length(unique(journal_mesh$TA))
num_docs_per_term <- journal_mesh %>% group_by(MH) %>% summarize(num_docs = length(unique(TA)))

# Okay, now we need to merge all of this information into the journal_mesh
journal_mesh <- journal_mesh %>% as.data.frame()
mhtfidf <- merge(journal_mesh, doc_len)
mhtfidf$total_num_docs <- total_num_docs
mhtfidf <- merge(mhtfidf, num_docs_per_term)

# Now let's calculate the tfidf metric

mhtfidf$tf <- mhtfidf$count / mhtfidf$doc_length
mhtfidf$idf <- log(mhtfidf$total_num_docs / mhtfidf$num_docs)

mhtfidf$tfidf <- mhtfidf$tf * mhtfidf$idf

write.csv(mhtfidf, "results/biological_sciences_mesh_tfidf_2015.csv", row.names = FALSE)
```

Okay, so we've calculated tf-idf weight which gives us some idea of how important a term is to a 
document, which in this case is a journal. Now, since the subject of our streamgraph is the JBC,
we will use the terms in the JBC with the highest tf-idf index as our representative terms for the
JBC. These terms define JBC's scientific niche.


```r
jbc <- mhtfidf %>% filter(TA == "J Biol Chem") %>% arrange(-tfidf) %>%
  slice(1:100)

write.csv(jbc, "results/jbc_characteristic_terms.csv", row.names = FALSE)
```
