setwd("biomarker/")
ncol(finf <- file.info(dir())) 
head(finf$size)

failedDownload <- finf[finf$size<3500000,]

files <- rownames(failedDownload)

gsub('.*-([0-9]+).*','\\1',files )


sub('.*-([0-9]+).*','\\1',files)
# load library
library(stringr)


# prepare regular expression
regexp <- "[[:digit:]]+"

# process string
downloadFile <- head(str_extract(files, regexp))

downloadFile[3] <-3e+05
files1 <- c(763000)
#query <- "\"0000/01/01\"[PDAT]:\"3000/12/31\"[PDAT]"
query <- "biomarker"

# Upload the PMIDs for this search to the History server
pmids <- esearch(query, "pubmed", usehistory = TRUE)
head(pmids)
#customFetch(pmids,outfile = "pubmedArticles4.txt")
## Not run:
# Fetch the records


setwd("biomarker/")

for (i in files1) {
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