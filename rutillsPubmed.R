#
# combine esearch and efetch
#
# Download PubMed records that are indexed in MeSH for both 'Chlamydia' and 
# 'genome' and were published in 2013.
#require("devtools")
#install_github("gschofl/reutils")
library(reutils)
library(XML)


#####custome efetch function 

customFetch <- function (uid, db = NULL, rettype = NULL, retmode = NULL, outfile = NULL, 
          retstart = NULL, retmax = NULL, querykey = NULL, webenv = NULL, 
          strand = NULL, seqstart = NULL, seqstop = NULL, complexity = NULL,start=0) 
{
  params <- parse_params(uid, db, querykey, webenv)
  
  r <- ncbi_retrieval_type(params$db, rettype %||% "", retmode)
  if (is.null(retmax)) {
    retmax <- Inf
  }
  if (retmax > 500 && (is.finite(params$count) && (params$count > 
                                                   500))) {
    print("true")
    if (is.null(outfile)) {
      stop("You are attempting to download more than 500 records.\n", 
           "       efetch() will download them iteratively in batches.\n", 
           "       Specify an 'outfile' where they can be saved to.", 
           call. = FALSE)
    }
    if (is.na(webenv(uid))) {
      stop("You are attempting to download more than 500 records.\n", 
           "       Use the NCBI history server to store the UIDs and\n", 
           "       specify an 'outfile' where the data can be saved to.", 
           call. = FALSE)
    }
    retstart <- start
    retmax <- 500
    out <- file(outfile, open = "w+")
    on.exit(close(out))
    while (retstart < params$count) {
      message("Retrieving UIDs ", retstart + 1, " to ", 
              retstart + retmax)
      ans <- .efetch("GET", db = params$db, id = NULL, 
                     query_key = params$querykey, WebEnv = params$webenv, 
                     retmode = r$retmode, rettype = r$rettype, retstart = retstart, 
                     retmax = retmax, strand = strand, seq_start = seqstart, 
                     seq_stop = seqstop, complexity = complexity)
      writeLines(ans$get_content("text"), con = out)
      retstart <- retstart + retmax
      Sys.sleep(0.33)
    }
    invisible(outfile)
  }
  else {
    if (!is.null(outfile)) {
      print(paste0("working",retstart))
      out <- file(outfile, open = "wt")
      on.exit(close(out))
      ans <- .efetch(method = if (length(params$uid) < 100) 
        "GET"
        else "POST", db = params$db, id = .collapse(params$uid), 
        query_key = params$querykey, WebEnv = params$webenv, 
        retmode = r$retmode, rettype = r$rettype, retstart = retstart, 
        retmax = retmax, strand = strand, seq_start = seqstart, 
        seq_stop = seqstop, complexity = complexity)
      writeLines(ans$get_content("text"), con = out)
      invisible(outfile)
    }
    else {
      .efetch(method = if (length(params$uid) < 100) 
        "GET"
        else "POST", db = params$db, id = .collapse(params$uid), 
        query_key = params$querykey, WebEnv = params$webenv, 
        retmode = r$retmode, rettype = r$rettype, retstart = retstart, 
        retmax = retmax, strand = strand, seq_start = seqstart, 
        seq_stop = seqstop, complexity = complexity)
    }
  }
}



#######end 

####define environment of function 

environment(customFetch) <- environment(efetch)
environment(customFetch)
####end


#query <- "\"0000/01/01\"[PDAT]:\"3000/12/31\"[PDAT]"
query <- "biomarker"

# Upload the PMIDs for this search to the History server
pmids <- esearch(query, "pubmed", usehistory = TRUE)
head(pmids)
#customFetch(pmids,outfile = "pubmedArticles4.txt")
## Not run:
# Fetch the records

setwd("biomarker/")
x <- seq(from=326500, to=767230, by=500)
for (i in x ) {
  print(i)
  tryCatch(#print(paste0("normal",i))
            customFetch(pmids,outfile = paste0("pubmedArticles",i,".txt"),start=i,retmax=500,retstart=i), 
            warning=function(w){   
             print("warning")
              customFetch(pmids,outfile = paste0("pubmedArticles",i,".txt"),start=i,retmax=500,retstart=i)
            }, 
            error=function(e) {     
             print("error")
              customFetch(pmids,outfile = paste0("pubmedArticles",i,".txt"),start=i,retmax=500,retstart=i)
            }, 
            finally={
              print("final")
              #customFetch(pmids,outfile = paste0("pubmedArticles",i,".txt"),start=i,retmax=500,retstart=i)
            });
  
  
}
#articles <- efetch(pmids,outfile = "pubmedArticles3.txt",retmax=500)

#articles <- readLines("pubmedArticles4.txt")

#library("methods")
#library(jsonlite)
#exportJson<- toJSON(articles)
#write(exportJson, file="export.JSON")



