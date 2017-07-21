
#required libraries 
library(caret)
library(corrplot)
library(xlsx)
library(RWeka)
library(PerformanceAnalytics)
#reading data
refineData <- read.xlsx2("refinedRoadAccident.xlsx",1,header=TRUE)

#head(refineData,use="pairwise.complete.obs")

#refineData <- cbind(c(1:lengths(refineData["Accident.Year"])),refineData ) #add number of accident as increment with row

#head(refineData)
#colnames(refineData)[1]<-c("Id")

head(refineData)
#refineData <- sapply(refineData,as.integer)
summary(refineData) # summary of data
#refineData <- apply(refineData, 1,as.integer)
head(summary(refineData))

xtabs(~refineData$SEX_CASU, data = refineData)

# check for any correlation

corData<-cor(sapply(refineData,as.numeric),use="pairwise.complete.obs")

# "flatten" that table
#flattenSquareMatrix(coreData)

# plot the data
corrplot(corData, method = "circle") #plot correlation matrix

chart.Correlation(corData)

corrplot(corData, order = "hclust")
#visualize the matrix, clustering features by correlation index.

highlyCor <- findCorrelation(corData, 0.40)

#Apply correlation filter at 0.40,
#head(highlyCor)
#then we remove all the variable correlated with more 0.40.
datMyFiltered <- corData[,-highlyCor]
corData <- cor(datMyFiltered)
corrplot(corData,order = "hclust")



#PCA
#install.packages("FactoMineR")
library(FactoMineR)
# PCA with function PCA


pca <- PCA(corData, scale.unit=TRUE, ncp=5, graph=T)
#scale all the features,  ncp: number of dimensions kept in the results (by default 5)

summary(pca)
plot(pca)
#plot of PCA
