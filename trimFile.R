#connect to internet 

options(download.file.method="wget")

library(RANN)
#set file path
setwd("/Users/naru/Documents/R_workshop/UTRClassify/data")

# triming dataframe
oncoGenes <- read.csv("OC5UTRWithoutDupliacates.csv")[1:400,]
length(oncoGenes$UTR_LENGTH)
head(oncoGenes)
#oncoGenes["CLASS"] <- "OncoGene"
head(oncoGenes)

# removing UTR IDs from dataFrame
oncoGeneWithoutId<-oncoGenes[,!(names(oncoGenes) %in% c("X","UTR_ID"))]
head(oncoGeneWithoutId)

#triming dataFrame
houseKeepingGenes <- read.csv("HK5UTRWithoutDupliacates.csv")[1:400,]
length(houseKeepingGenes$UTR_LENGTH)
head(houseKeepingGenes)
#houseKeepingGenes["CLASS"] <- "houseKeepingGene"
head(houseKeepingGenes)
# removing UTR IDs from dataFrame
houseKeepingGenesWithoutId<-houseKeepingGenes[,!(names(houseKeepingGenes) %in% c("X","UTR_ID"))]
head(houseKeepingGenesWithoutId)

# combing dataframes
dim(oncoGeneWithoutId)
dim(houseKeepingGenesWithoutId)
trainingData <- rbind(oncoGeneWithoutId,houseKeepingGenesWithoutId)
dim(trainingData)

# teasting data 

testingData <- read.csv("OC5UTRWithoutDupliacates.csv")[401:450,]
#testingData <- read.csv("HK5UTRWithoutDupliacates.csv")[401:425,]
testingData
#testingData["CLASS"] <- "OncoGene"
# removing UTR IDs from dataFrame
testingData <- testingData[,!(names(testingData) %in% c("X","UTR_ID"))]

cl=factor(c(rep("OncoGene",400),rep("HouseKeepingGene",400)))
#knn classification 
dim(trainingData)
dim(testingData)

#nn2(trainingData, query = testingData, k = min(100, nrow(trainingData)), treetype = c("kd", "bd"),    searchtype = c("standard", "priority", "radius"), radius = 0, eps = 0)

neighbour <- nn2(trainingData, query = testingData, k = min(100, nrow(trainingData)), treetype = "kd",searchtype = "standard",radius = 0, eps = 0)

#adding classes name for classification


for (i in 1:length(neighbour$nn.idx[,1])){
    
 # length(which(400 < neighbour$nn.idx[1,]))

  #  length(which(400 >= neighbour$nn.idx[1,]))
  print(length(which(400 >= neighbour$nn.idx[i,])))
  print(length(which(400 < neighbour$nn.idx[i,])))
  onco <- (1/2)*(length(which(400 >= neighbour$nn.idx[i,]))/400)
  house <-(1/2)*(length(which(400 < neighbour$nn.idx[i,]))/400)
  print(onco)
  print(house)

  if(house < onco){
     print("oncoGene")}
  if(house > onco){
     print("houseKeepingGene")}
  if(house == onco){
    print("Equal")}
}

#length(neighbour$nn.idx[,1])


#knn(trainingData, testingData, cl, 100,l=3)
#knn(trainingData, testingData,cl , k = 100, l = 3, prob = TRUE, use.all = TRUE)

#write.csv(file="traningData5UTR.csv",trainingData)

