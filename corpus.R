#author narumeena
#description create corpus from excell file where each raw treated as document
#date 13/12/2015



#Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
#install.packages(Needed, dependencies=TRUE)   

#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")   





#cname <- file.path("~", "Documents/R_workshop/SpamFilter", "data")   
#cname 


#dir(cname)
#install.packages("pblapply", type="source")
library(tm)       
#library(xlsx)
#install.packages("openxlsx")
library(openxlsx)

#install.packages("devtools")
#library(devtools)
#dev_mode(on=T)
#install_github("cran/pbapply")
# change this file location to suit your machine
file_loc <- "/Users/naru/Documents/R_workshop/SpamFilter/spameData1.xlsx"
# change TRUE to FALSE if you have no column headings in the CSV

#source("xlsxToR.R")

#x <- xlsxToR(file_loc, header = TRUE)


x <- read.xlsx(file_loc, sheet = 1, startRow = 1, colNames = TRUE,)

x$Message
head(x)
docs <- Corpus(DataframeSource(x))
#dtm <- DocumentTermMatrix(docs)

#docs <- Corpus(DirSource(cname))   

summary(docs) 

inspect(docs[2])[1]
docs[1]

docs <- tm_map(docs, removePunctuation)   
 inspect(docs[3]) # Check to see if it worked. 
 
 
 
 for(j in seq(docs))   
 {   
   docs[[j]] <- gsub("/", " ", docs[[j]])   
   docs[[j]] <- gsub("@", " ", docs[[j]])   
   docs[[j]] <- gsub("\\|", " ", docs[[j]])   
 }   
  inspect(docs[1]) # You can check a document (in this case the first) to see if it worked.   
  
  
  docs <- tm_map(docs, removeNumbers)   
   inspect(docs[3]) # Check to see if it worked.  

docs <- tm_map(docs, tolower) 
 inspect(docs[3]) # Check to see if it worked. 

docs <- tm_map(docs, removeWords, stopwords("english"))  


# For a list of the stopwords, see:   
# length(stopwords("english"))   
# stopwords("english")   
docs <- tm_map(docs, removeWords, stopwords("english"))   
 inspect(docs[3]) # Check to see if it worked.   

for (j in seq(docs))
{
  docs[[j]] <- gsub("qualitative research", "QDA", docs[[j]])
  docs[[j]] <- gsub("qualitative studies", "QDA", docs[[j]])
  docs[[j]] <- gsub("qualitative analysis", "QDA", docs[[j]])
  docs[[j]] <- gsub("research methods", "research_methods", docs[[j]])
}

library(SnowballC)   
docs <- tm_map(docs, stemDocument) 

docs <- tm_map(docs, stripWhitespace)   
 inspect(docs[3]) # Check to see if it worked. 

docs <- tm_map(docs, PlainTextDocument)  

dtm <- DocumentTermMatrix(docs)   
dtm  
# 
# remove <- removeSparseTerms(dtm, 0.4)
# dim(remove)
# head(remove)
# inspect(dtm)

tdm <- TermDocumentMatrix(docs)   
tdm  

freq <- colSums(as.matrix(dtm))   
length(freq)  


ord <- order(freq)  


m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="dtm.csv")  
