

#data <- read.csv("data/file_list.txt",sep="\t",header=FALSE)

#install.packages("rentrez")
#install.packages("XML")
#install.packages("reutils")
library(reutils)
library(rentrez)
library(XML)


"NCID_1_361855193_130.14.18.97_5555_1456827587_1079916785_0MetA0_S_HStore"
#library(stringr)
#data<- cbind(data,str_split_fixed(data$V4, ":", 2))

#head(data)
#your.ids <- c("26386083","26273372","26066373","25837167","25466451","25013473","")
#your.ids <- data['2']
#head(your.ids)
#fetch.pubmed <- efetch(your.ids, db = "pubmed", retmode = 'xml')
#using web_history

###pubmedSearch
## Not run: 
pubmedIds <- entrez_search(db="pubmed", "\"0000/01/01\"[PDAT]:\"3000/12/31\"[PDAT]", retmax=25803988)

upload <- entrez_post(db="pubmed", id=pubmedIds$ids)


fetch.pubmed <- entrez_fetch(db = "pubmed",
                             web_history = upload,   rettype = "xml", parsed = T)
# rentrez function to get the data from pubmed db
#fetch.pubmed <- entrez_fetch(db = "pubmed", id = your.ids,
#                             rettype = "xml", parsed = T)

#install.packages("jsonlite")

library(jsonlite)
datalist <- xmlToList(fetch.pubmed)
head(datalist)
exportJson<- toJSON(datalist)
write(exportJson, file="export.JSON")
#save(exportJson, file="export.JSON",ascii=TRUE)

# Extract the Abstracts for the respective IDS.  
#abstracts = xpathApply(fetch.pubmed, '//PubmedArticle//Article', function(x)
#  xmlValue(xmlChildren(x)$Abstract))
# Change the abstract names with the IDS.
#names(abstracts) <- your.ids
#abstracts
#col.abstracts <- do.call(rbind.data.frame,abstracts)
#dim(col.abstracts)
#write.csv(col.abstracts, file = "test.csv")