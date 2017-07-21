# internet connection 

options(download.file.method="wget")
#install.packages("lsa")
# lloding cosine fucntion library in session 
library(lsa)
#library(dplyr)
#library(doParallel)
#library(plyr)
#install.packages("doMC")
#library(doMC)

uniqeIds <-function(d){
  #local variables
  a = list() 
  b = list()
  # loop
  for (i in 1:length(d$Var1)) {
    var1 = as.character(d$Var1[i])
    var2 = as.character(d$Var2[i])
    # repitative Ids
    if(!is.element(var1, a) & !is.element(var1, b) & (var1!=var2)){
      a[length(a)+1] = var1
    }
    # all ids with repetative Ids
    if(!is.element(var2, b)){
      b[length(b)+1] = var2
      
    }
  }
  # return Ids without duplication 
  return( b[!(b %in% a)] )
}

library(xlsx)
# reading function
setwd("/Users/naru/Documents/R_workshop/UTRClassify/data")
houseKeepingGenes <- read.table("HK3UTR.xls", sep="\t",header=TRUE)
head(houseKeepingGenes)
houseKeepingGenes
#converting as matrix 
data = as.matrix(houseKeepingGenes)
data
#normlizing data 
normlizeData<- t(apply(houseKeepingGenes[,][3:length(houseKeepingGenes[1,])], 1, function(x)(x-min(x))/(max(x)-min(x))))

#name rows as UTRids
rownames(normlizeData) <-houseKeepingGenes[,1]
#transform row into columes for cosine calculation 
transform_matrix<-t(normlizeData)

as.list(colnames(transform_matrix))
# setting parallel cluster 
#doMC::registerDoMC(cores=4) 
#give columns name using UTR ids 
#colnames(transform_matrix) <- file[,1]
# cosineData <- ddply(transform_matrix, cosine(transform_matrix), .parallel = TRUE)  # parallel
# #cosineData <- cosine(transform_matrix)
# cosineData
# #out <- data.frame(X1 = rownames(cosineData)[-1],X2 = head(colnames(cosineData), -1),Value = cosineData[row(cosineData) == col(cosineData) + 1])
# #head(out)
# # converting matrix into dataframe 
# 
# cosineData <-as.data.frame(as.table(cosineData))
# head(cosineData)
# 
# 
# 
# #filterCosineDataSameId<-filter(cosineData, Var1 == Var2 & Freq >= 0.999)
# #filterCosineDataSameId
# #filterCosineDataDiffrentId<-filter(cosineData, Var1 != Var2 & Freq >=0.999)
# #filterCosineDataDiffrentId
# filterCosineDataWithSameFreq <- ddply(cosineData, filter(cosineData,  Freq >= 0.999), .parallel = TRUE) # parallel
# #filterCosineDataWithSameFreq<-filter(cosineData,  Freq >= 0.999)
# d <-filterCosineDataWithSameFreq
# 
# 
# result <- ddply(d,uniqeIds(d), .parallel = TRUE)
# #result<-uniqeIds(d)
# result
# output <- ddply(result,houseKeepingGenes[houseKeepingGenes$UTR_ID %in% result, ], .parallel = TRUE)  # parallel
# #output<-houseKeepingGenes[houseKeepingGenes$UTR_ID %in% result, ]
# output
# #write.csv(file="HK5UTRWithoutDupliacates.csv", output)

write.csv(file="HK3UTRWithoutDupliacates.csv", houseKeepingGenes[houseKeepingGenes$UTR_ID %in% uniqeIds(filter(as.data.frame(as.table(cosine(transform_matrix))),  Freq >= 0.999)), ])
