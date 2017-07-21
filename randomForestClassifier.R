
#connect to internet 

#options(download.file.method="wget")

#install.packages("randomForest")
library(randomForest)


#set file path
setwd("/Users/naru/Documents/R_workshop/UTRClassify/data")

# triming dataframe
oncoGenes <- read.csv("OC5UTRWithoutDupliacates.csv")[1:400,]
length(oncoGenes$UTR_LENGTH)
head(oncoGenes)
oncoGenes["CLASS"] <- "OncoGene"
levels(oncoGenes$CLASS) <- 'OncoGene'
#oncoGenes$CLASS <-factor(oncoGenes$CLASS,levels=c('oncoGene'))
head(oncoGenes)

# removing UTR IDs from dataFrame
oncoGeneWithoutId<-oncoGenes[,!(names(oncoGenes) %in% c("X","UTR_ID"))]
head(oncoGeneWithoutId)

#triming dataFrame
houseKeepingGenes <- read.csv("HK5UTRWithoutDupliacates.csv")[1:400,]
length(houseKeepingGenes$UTR_LENGTH)
head(houseKeepingGenes)
houseKeepingGenes["CLASS"] <- "houseKeepingGene"
levels(houseKeepingGenes$CLASS) <- 'houseKeepingGene'
houseKeepingGene#s$CLASS <-factor(houseKeepingGenes$CLASS,levels=c('houseKeepingGene','oncoGene'))
oncoGenes$CLASS <-factor(oncoGenes$CLASS,levels=c('oncoGene'))
head(houseKeepingGenes)
# removing UTR IDs from dataFrame
houseKeepingGenesWithoutId<-houseKeepingGenes[,!(names(houseKeepingGenes) %in% c("X","UTR_ID"))]
head(houseKeepingGenesWithoutId)


# combing dataframes
dim(oncoGeneWithoutId)
dim(houseKeepingGenesWithoutId)
trainingData <- rbind(oncoGeneWithoutId,houseKeepingGenesWithoutId)
trainingData$CLASS
dim(trainingData)
head(trainingData)
class.rf <- randomForest(CLASS ~ ., data=trainingData,importance=TRUE,
                         proximity=TRUE)

## Using different symbols for the classes:
MDSplot(class.rf,trainingData$CLASS)

#PCA 
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
log.ir <- log(trainingData[, 1:90])
ir.species <- iris[, 5]
head(log.ir)
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
#Sample.scaled.2 <- data.frame(t(na.omit(t(log.ir))))
#log.ir <- scale(log.ir) 
#log.ir[is.nan(log.ir)] <- 0 
log.ir <- data.frame(t(na.omit(t(log.ir))))
ir.pca <- prcomp(log.ir, retx=TRUE) 
#ir.pca <- prcomp(trainingData[,-91],center = TRUE,scale. = TRUE) 
#plot(trainingData[,-91])
library(devtools)
#library(httr)
#set_config(
#  use_proxy(url="", port=0, username="",password="")
#)
#install_github("ggbiplot", "vqv")
#install.packages(ggbiplot)
library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
              groups = trainingData$CLASS, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
plot(ir.pca,type = "1")


library(devtools)
#install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
              groups = ir.species, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


#random Forest
#install.packages("iris")
data(iris)

data(imports85)
head(imports85)
imp85 <- imports85[,-2] # Too many NAs in normalizedLosses.
head(imp85)
imp85 <- imp85[complete.cases(imp85), ]
## Drop empty levels for factors.
imp85[] <- lapply(imp85, function(x) if (is.factor(x)) x[, drop=TRUE] else x)

stopifnot(require(randomForest))
price.rf <- randomForest(price~., imp85, do.trace=10, ntree=100)
print(price.rf)
numDoors.rf <- randomForest(numOfDoors ~ ., imp85, do.trace=10, ntree=100)
print(numDoors.rf)


## Classification:
##data(iris)
set.seed(71)
head(iris)
class(iris)
iris.rf <- randomForest(CLASS ~ ., data=trainingData, importance=TRUE,
                        proximity=TRUE)



## The `unsupervised' case:
set.seed(17)
iris.urf <- randomForest(trainingData[,-91])
iris[, -5]
trainingData[,-91]
trainingData$CLASS
MDSplot(iris.urf, iris$Species)

## stratified sampling: draw 20, 30, and 20 of the species to grow each tree.
(iris.rf2 <- randomForest(iris[1:4], iris$Species, 
                          sampsize=c(20, 30, 20)))

