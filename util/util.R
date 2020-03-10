retrieve_journal_identifiers <- function(srch_string) {
  print(srch_string)
  # jrnls <- esearch("\"biological science disciplines\"[MeSH Major Topic] AND \"periodicals\"[Publication Type] AND (ncbijournals[All Fields] AND English[lang])",
  # db="nlmcatalog",retmax=5000)
  jrnls <- esearch(srch_string, db="nlmcatalog", retmax=5000)
  jrnls <- esummary(jrnls,db="nlmcatalog")
  jrnls <- content(jrnls,as="parsed")
  
  jids <- do.call(rbind, sapply(names(sapply(jrnls,names)),function(x) {
    jname <- x
    j <- jrnls[[x]]
    if("ISSNList.ISSNInfo.issn" %in% names(j)) { issn <- j$ISSNList.ISSNInfo.issn }
    else{issn <- NA}
    
    if("TitleMainList.TitleMain.SortTitle" %in% names(j)) { title <- j$TitleMainList.TitleMain.SortTitle}
    else{title <- NA}
    
    if("MedlineTA" %in% names(j)) { ta <- j$MedlineTA }
    else{ta <- NA}
    
    data.frame("uid"=x,"issn"=issn, "title"=title, "ta"=ta, stringsAsFactors=F)
  },
  simplify=F))}

retrieve_article_identifiers <- function(j_ids, years) {
  count <- length(j_ids)*length(years)
  # pb <- prog_bar_initialize("retrieve_article_identifiers",count,1:count)
  
  art_ids <- data.frame("issn"=character(0),"search_year"=numeric(0),"article_count"=numeric(0), "uids"=character(0))
  
  article_query_log <- file("./logs/esearch_log.txt",open="wt")
  sink(article_query_log, type="message")
  
  
  for(i in 1:length(years)) {
    for(j in 1:length(j_ids)) {
      if(j%%100 == 0) { prog_bar_update("retrieve_article_identifiers",pb[i*j]) }
      pmids <- esearch(paste(j_ids[[j]],"[TA] and ",years[[i]],"[pdat]",sep=""),db="pubmed", retmax=10000)
      pmids <- paste(uid(pmids),collapse=";")
      
      
      temp <- data.frame("issn"=j_ids[[j]], "search_year"=years[[i]],"article_count"=length(pmids),"pmids"=pmids)
      art_ids <- rbind(art_ids,temp)
      
      Sys.sleep(time=0.2)
    }
  }
  
  sink(type="message")
  close(article_query_log)
  
  return(art_ids) }

medline_parser <- function(file_name){
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
  rec <- as.data.frame(medline_records[1])
  rec[sapply(rec, is.factor)] <- lapply(rec[sapply(rec, is.factor)], 
                                        as.character)
  for(record in medline_records[-1]) {
    rec <- bind_rows(list(rec, as.data.frame(record, stringsAsFactors = FALSE)))
  }
  return(rec)}

prog_bar_initialize <- function(func_name, number_of_queries, query_segments) {
  
  pbPost("note",paste0(func_name," has initiated"),
         paste0("With a total of ",number_of_queries," queries."))
  
  progs <- seq(from=0, to=1, by=(1/length(query_segments)))}

prog_bar_update <- function(func_name, prog) {
  hashes <- round(prog * 10,digits=0)
  prog_text <- paste("|",paste(rep(">",hashes),collapse=""),paste(rep("<",(10-hashes)),collapse=""),"|",sep="")
  prog_text <- paste(prog_text,round(prog,digits=2),sep=" ")
  
  pbPost("note",paste(round(prog,digits=2),func_name,"running",sep=" "),body=prog_text) }

efetch_batch <- function(uids, chunk_size=500) {
  
  count <- length(uids)
  n_chunks <- count%/%chunk_size
  retstart <- seq(from=1, to=(n_chunks*chunk_size), by=chunk_size)
  
  # pb <- prog_bar_initialize("efetch_batch",count,retstart)
  
  res <- efetch(uids,db="pubmed",rettype="medline",retmax=chunk_size)
  res <- content(res,as="parsed")
  res <- paste(res,collapse="\n")
  res <- medline_parser(res)
  
  
  
  for(i in 1:length(retstart)) {
    print(i)
    
    
    # if(i%%20 == 0) { prog_bar_update("efetch_batch",pb[i]) }
    
    temp_res <- efetch(uids,db="pubmed",rettype="medline",
                       retmax=chunk_size,retstart=retstart[i])
    temp_res <- content(temp_res,as="parsed")
    temp_res <- paste(temp_res,collapse="\n")
    temp_res <- medline_parser(temp_res)
    
    if(i%%10 == 0) {
      write.csv(res,paste0("./res/res",i,".csv"))
      res <- temp_res
    }
    else{ res <- bind_rows(list(res,temp_res)) }
    
    Sys.sleep(time=0.1)
  }  
  # prog_bar_update("efetch_batch",1)
  write.csv(res,paste0("./res/res",i,".csv"))
}

merge_mesh_by_journals <- function(mesh, j_ids, art_ids) {
  data <- merge(art_ids,j_ids,by="issn")
  data <- data[complete.cases(data),]
  
  data$keywords <- lapply(data$pmids, function(x) {
    pmids <- unlist(strsplit(x,";"))
    unlist(paste(mesh[which(names(mesh) %in% pmids)],collapse=";"))
  })
  data$keywords <- as.character(data$keywords)
  
  data <- data[which(nchar(data$keywords)!=0),]
  data$ta <- as.character(data$ta)
  
  return(data)
}

split_journal_keywords <- function(sdata) {
  sdata <- sdata[,c("ta","keywords")]
  
  kws <- data.frame("ta"=character(0), "mesh"=character(0))
  
  for(i in 1:length(sdata$ta)) {
    tkey <- unlist(strsplit(sdata[i,"keywords"],";"))
    tkws <- data.frame("ta"=sdata[i,"ta"],"mesh"=tkey)
    kws <- rbind(kws,tkws)
  }
  
  return(kws)
}

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


assemble_query_w_subheadings <- function(query_term, subheadings, year) {
  query <- paste0("\"", query_term, "/", subheadings[[1]],"\"[Mesh]")
  for(i in 2:length(subheadings)) {
    query <- paste0(query, " OR \"", query_term, "\" and ", year, "[pdat]")
  }
  return(query)
}

make_query <- function(term) {
  subheadings <- unlist(strsplit(term, "/"))[-1]
  if(length(subheadings) > 1) {
    query_term <- unlist(strsplit(term, "/"))[[1]]
    query <- assemble_query_w_subheadings(query_term, subheadings, years[i])
  } else {
    query <- niche_terms$MH[j]
  }
  return(query)
}

