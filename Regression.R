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

#removing empty levels from data
levels(refineData$SEX_CASU)
refineData <- refineData[!(is.na(refineData$AGE_CASU) | refineData$AGE_CASU==""), ]
refineData <- with(refineData, refineData[!(nzchar(SEX_CASU) | is.na(SEX_CASU)), ])

levels(refineData$SEX_CASU)


## Linear regression:

LinearRegression(~ ., data =refineData )

## Using standard data set 'mtcars'.
LinearRegression(mpg ~ ., data = mtcars)
## Compare to R:
step(lm(mpg ~ ., data = mtcars), trace = 0)
## Using standard data set 'chickwts'.
LinearRegression(weight ~ feed, data = chickwts)
## (Note the interactions!)
## Logistic regression:
## Using standard data set 'infert'.
STATUS <- factor(infert$case, labels = c("control", "case"))
Logistic(STATUS ~ spontaneous + induced, data = infert)

## Compare to R:
glm(STATUS ~ spontaneous + induced, data = infert, family = binomial())
## Sequential minimal optimization algorithm for training a support
## vector classifier, using am RBF kernel with a non-default gamma
## parameter (argument '-G') instead of the default polynomial kernel
## (from a question on r-help):
SMO(Species ~ ., data = iris,
    control = Weka_control(K =
                             list("weka.classifiers.functions.supportVector.RBFKernel", G = 2)))
## In fact, by some hidden magic it also "works" to give the "base" name
## of the Weka kernel class:
SMO(Species ~ ., data = iris,
    control = Weka_control(K = list("RBFKernel", G = 2)))
