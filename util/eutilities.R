###########################################################################
# Michael L. Bernauer
# mlbernauer@gmail.com
# 12/14/2014
#
# Module for retrieving and parsing PubMed Medline formatted results from
# NCBI.
#
# USAGE:
# source('medline.R')
# medline_records <- medline("/home/user/Downloads/pubmed_results.txt")
#
# USAGE:
# query <- "\"unversity of new mexico\"[AD] AND \"pharmacy\"[AD]"
# entrez_results < entrez_fetcher(query, "pubmed", "medline")
#
# Results can be parsed with medline_parser()
# medline_results <- medline_parser(entrez_results)
############################################################################

library(RCurl)
library(XML)

# Function for parsing medline files downloaded from pubmed
# or medline formatted text returned from the entrez_fetcher()
# function.
medline_parser = function(file_name){
  if(file.exists(file_name)){
    lines <- readLines(file_name)
  }
  else {
    lines <- strsplit(file_name, "\n")[[1]]
  }
  medline_records <- list()
  key <- 0
  record <- 0
  for(line in lines){
    header <- sub(" {1,20}", "", substring(line, 1, 4))
    value <- sub("^.{6}", "", line)
    if(header == "" & value == ""){
      next
    }
    else if(header == "PMID"){
      record = record + 1
      medline_records[[record]] <- list()
      medline_records[[record]][header] <- value
    }
    else if(header == "" & value != ""){
      medline_records[[record]][key] <- paste(medline_records[[record]][key], value)
    }
    else{
      key <- header
      if(is.null(medline_records[[record]][key][[1]])){
        medline_records[[record]][key] <- value
      }
      else {
        medline_records[[record]][key] <- paste(medline_records[[record]][key], value, sep=";")
      }
    }
  }
  return(medline_records)
}

# Function for retrieving PubMed search results to be parsed with
# medlineParser
entrez_fetcher = function(query_string, db, type){
  base <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  query <- gsub(" ", "+", query_string)
  query <- paste("esearch.fcgi?db=", db, "&term=", query, "&usehistory=y", sep="")
  url <- paste(base, query, sep="")
  url_result <- getURL(url)
  
  xml_data <- xmlToList(xmlParse(url_result))
  web <- xml_data["WebEnv"][[1]]
  key <- xml_data["QueryKey"][[1]]
  
  # Assemble Efetch URL
  fetch <- paste(base, "efetch.fcgi?db=", db, "&query_key=", key,
                 "&WebEnv=", web, "&rettype=", type, "&retmode=text", sep="")
  data <- getURL(fetch)
  return(data)
}