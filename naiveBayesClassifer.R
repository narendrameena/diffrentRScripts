
#connect to internet 

options(download.file.method="wget")

#install.packages("e1071")
#install.packages("mlbench")
#install.packages("class")
#install.packages("RANN")
library(e1071)
library(class)
library(RANN)
#library(mlbench)

#set file path
setwd("/Users/naru/Documents/R_workshop/UTRClassify/data")

# training data
traningData <- read.csv("traningData5UTR.csv")
head(trainingData)

# teasting data 

#testingData <- read.csv("OC5UTRWithoutDupliacates.csv")[401:450,]
testingData <- read.csv("HK5UTRWithoutDupliacates.csv")[401:1000,]
testingData
#testingData["CLASS"] <- "OncoGene"
# removing UTR IDs from dataFrame
testingData <- testingData[,!(names(testingData) %in% c("X","UTR_ID"))]

model <- naiveBayes(CLASS ~ ., data = trainingData)
model
predict(model, testingData)
predict(model, testingData, type = "raw")



pred <- predict(model, testingData)
table(pred, testingData$CLASS)

## using laplace smoothing:
model <- naiveBayes(CLASS ~ ., data = testingData, laplace = 3)
pred <- predict(model, testingData[,-1])
table(pred, testingData$CLASS)


# manually naive bayes classifier
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

