#install.packages("topicmodels")
#install.packages("tm.plugin.mail")

library(stringr)
library(plyr)
library(tm)
library(tm.plugin.mail)


library(SnowballC)
library(topicmodels)




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
  return(strtrim(date, 25))
  }else{return('NA')}
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
  msg <- msg.vec[seq(which(msg.vec == "")[1] + 1, length(msg.vec), 1)]
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
  }else{return('NA')}
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
  from <- get.from(full.msg)
  subj <- get.subject(full.msg)
  msg <- get.msg(full.msg)
  print(msg)
  #return(c(date, from, subj, msg, path))
  return(c(date,from, subj, msg, path))
}

setwd("/Users/naru/Documents/R_workshop/SpamFilter")
email.files <- list.files("data", ".ivanova", recursive=TRUE, full.names=TRUE)
#email.file <- list.files("data/", ".ivanova", recursive=TRUE, full.names=TRUE)[1]

#codepages <- setNames(iconvlist(), iconvlist())

#x <- lapply(codepages, function(enc) try(email.file))
#email_txts
#email_txts[200]
#parse.email(paste(data.path,email_txts[1]))
#result <- c(parse.email("/Users/naru/Documents/R_workshop/SpamFilter/data/1423436693.12029_23.ivanova"))
#write.csv(result, file = "Output.csv", append = T)
#parse.email(email.file)


easyham.path <-"/Users/naru/Documents/R_workshop/SpamFilter/data/"
easyham.docs <- dir(easyham.path)
#easyham.docs <-"/Users/naru/Documents/R_workshop/SpamFilter/data/1424574180.8026_37.ivanova"
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
#paste(easyham.path, p, sep="")
parse.email("/Users/naru/Documents/R_workshop/SpamFilter/data/1424574180.8026_37.ivanova")
easyham.parse <- lapply(easyham.docs, function(p) 
{parse.email(paste(easyham.path, p, sep=""))})

ehparse.matrix <- do.call(rbind, easyham.parse)
allparse.df <- data.frame(ehparse.matrix, stringsAsFactors=FALSE)
dim(allparse.df)
allparse.df[5]
names(allparse.df) <- c("Date", "From.EMail", "Subject", "Message", "Path")

#write.csv(allparse.df,file="spameDAta.csv")
library(xlsx)
write.xlsx(allparse.df, file="spameDAta.xlsx")
