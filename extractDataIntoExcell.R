#author narumeena
#description extract data frow raw files into excell
#date 12/12/2015






#install.packages("topicmodels")
#install.packages("tm.plugin.mail")

library(stringr)
library(plyr)
library(tm)
library(tm.plugin.mail)


library(SnowballC)
library(topicmodels)

options(max.print=1000000) 


# get date of email
get.date <- function(msg.vec) {
  date.grep <- grepl("^Date: |^Date:", msg.vec)
  #print(length(date.grep))
  date.grepl <- which(date.grep == TRUE)
  #print(date.grepl)
  date <- msg.vec[date.grep]
  if(length(date)>0){
  date <- strsplit(date, "\\+|\\-|: ")[[1]][2]
  #print(date)
  date <- gsub("^\\s+|\\s+$", "", date)
  #return(date)
  print(date)
  return(strtrim(date, 25))
  }else{return("NA")}
}

# get subject of email
get.subject <- function(msg.vec) {
  subj <- msg.vec[grepl("Subject: ", msg.vec)]
  if(length(subj) > 0) {
 
    return(strsplit(subj, "Subject: ")[[1]][2])
    
    #return(subj)
  }
  else {
    return("")
  }
}

#get the message of email
get.msg <- function(msg.vec) {
  print(length(msg.vec))
  print(seq(which(msg.vec == "" | is.na(NA))[1] + 1))
  #msg <- msg.vec[seq(which(msg.vec == "")[1] + 1, length(msg.vec), 1)]
  msg <- msg.vec[seq(which(msg.vec == "" | is.na(NA))[1], length(msg.vec), 1)]
  #print(paste(msg, collapse="\n"))gsub("[\r\n]", "", x)
  #return(paste(gsub("[\r\n]"," ",msg), collapse="\n"))
  return(paste(msg, collapse="\n"))

}

#get the sender email 
get.from <- function(msg.vec) {
  from <- msg.vec[grepl("From: ", msg.vec)]
  if(length(from)>0){
  from <- strsplit(from, '[":<> ]')[[1]]

  from <- from[which(from !="" & from !=" ")]
  
  return(from[grepl("@", from)][1])
  }else{return("NA")}
}
         
#fully message of email        
msg.full <- function(path) {
  #con <- file(path, open="rt",encoding="latin1")
  con <- file(path, open="rt",encoding="latin1")
  msg <- readLines(con)
  close(con)
  return(msg)
}

parse.email <- function(path) {
  full.msg <- msg.full(path)
  print(path)
  date <- get.date(full.msg)
  print(date)
  if(!is.na(date)){
  from <- get.from(full.msg)
  subj <- get.subject(full.msg)
  print(subj)
  msg <- get.msg(full.msg)
  print(msg)
  #return(c(date, from, subj, msg, path))
  return(c(date,from, subj, msg, path))
  }else
    {
      return(c("NA","NA", "NA", "NA", path))
      }
}




spamData.path <-"/Users/naru/Documents/R_workshop/SpamFilter/spamFilter/data/"

#spamData.path <- "/Users/naru/Documents/R_workshop/SpamFilter/data/"
spamData.docs <- dir(spamData.path,pattern = "*.ivanova|*.lorien|*.txt",recursive = TRUE)
#spamData.docs <-"/Users/naru/Documents/R_workshop/SpamFilter/data/1424574180.8026_37.ivanova"
spamData.docs <- spamData.docs[which(spamData.docs != "cmds")]
#paste(spamData.path, p, sep="")
#parse.email("/Users/naru/Documents/R_workshop/SpamFilter/SpamFilter/data/1072737771.26868_1121.txt")




#parse.email("/Volumes/narumeena/spamFilter/data/1420663271.17998_49.lorien")
spamData.parse <- lapply(spamData.docs, function(p) {parse.email(paste(spamData.path, p, sep=""))})

ehparse.matrix <- do.call(rbind, spamData.parse)



allparse.df <- data.frame(ehparse.matrix, stringsAsFactors=FALSE)
dim(allparse.df)
allparse.df[5]
names(allparse.df) <- c("Date", "From.EMail", "Subject", "Message", "Path")

#write.csv(allparse.df,file="spameDAta.csv")
#options(java.parameters = "-Xmx4000m")
options(java.parameters = "-Xmx10g" )
library(xlsx)
#setwd("/Volumes/narumeena/spamFilter")
write.xlsx(allparse.df, file="spameData1.xlsx")
write.csv(allparse.df, file="spameData1.csv")
write.table(allparse.df, file="spameData1.txt")
head(allparse.df)
Sys.which(R)
