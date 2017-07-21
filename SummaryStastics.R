#read test file 
data <- read.delim("test1.txt",header = TRUE)

#sorting fuction, mergesort algoritham 
mySort <- function(m)
{
  merge <- function(left, right)
  {
    result <- c()
    while(length(left) > 0 && length(right) > 0)
    {
      if(left[1] <= right[1])
      {
        result <- c(result, left[1])
        left <- left[-1]
      } else
      {
        result <- c(result, right[1])
        right <- right[-1]
      }         
    }
    if(length(left) > 0) result <- c(result, left)
    if(length(right) > 0) result <- c(result, right)
    #print(class(result))
    result
  }
  
  len <- length(m)
  if(len <= 1) m else
  {
    middle <- length(m) / 2
    left <- m[1:floor(middle)]
    right <- m[floor(middle+1):len]
    left <- mySort(left)
    right <- mySort(right)
    if(left[length(left)] <= right[1])
    {
      c(left, right)
    } else
    {
      merge(left, right)
    } 
  }
}
#mySort(c(787, 5275,  201  ,  4, 1065,   11)) 



#minimum function
myMin <- function(x) {
 # print(x)
  if(any(is.na(x))){
    x<- x[-which(is.na(x)==TRUE)]
  }
#  print(x)
  sortArray<-mySort(x)
 # print(class(sortArray[1]))
  sortArray[1]
}
myMin(data$rna_act_3utr)

#maximum function
myMax <- function(x) {
  #print(x)
  if(any(is.na(x))){
   x<- x[-which(is.na(x)==TRUE)]
  }
  print(x)
  sortArray<-mySort(x) # order list in increment order 
  print(sortArray)
  sortArray[length(sortArray)]
}
myMax(data$rna_act_3utr)
myMax(data$rna_res_trans)

#mean function
myMean <- function(x) {
  if(any(is.na(x))){
    x<- x[-which(is.na(x)==TRUE)]
  }
# x<- x[-which(is.nan(x)==TRUE)]
  total <- sum(x)
  n <- length(x)
  total/n
 
}
myMean(data$rna_act_3utr)

# median function
myMedian <- function(x) {
  if(any(is.na(x))){
    x<- x[-which(is.na(x)==TRUE)]
  }
  x<- mySort(x)
 
  medianNum <- length(x)/2
  
  if(medianNum%%1!=0){
    x[ceiling(medianNum)]
  }else
  {
    (x[medianNum] + x[medianNum+1])/2
  }
}
myMedian(data$rna_act_3utr)
myMedian(data$rna_res_trans)
myMedian(c(787, 5275,  201  ,  4, 1065)) 
median(c(787, 5275,  201  ,  4, 1065))

# standrd deviation function 

myStd <- function(x){
  if(any(is.na(x))){
    x<- x[-which(is.na(x)==TRUE)]
  }
  sqrt(sum((x - myMean(x))^2) / (length(x) - 1))
}
myStd(data$rna_act_3utr)
myStd(data$rna_res_trans)

# variance(population) function 
myVar <- function(x){
  if(any(is.na(x))){
    x<- x[-which(is.na(x)==TRUE)]
  }
  sum((x-myMean(x))^2)/(length(x)-1) 
}

myVar(data$rna_act_3utr)
myVar(data$rna_res_trans)

#summery function 
sumStat <- function(x) {
  if(is.numeric(x)){
    c(min = min(x,na.rm = TRUE), myMin = myMin(x),
    max = max(x,na.rm = TRUE), myMax = myMax(x),
    mean = mean(x,na.rm = TRUE), myMean = myMean(x),
    median = median(x,na.rm = TRUE), myMedain = myMedian(x),
    std = sd(x,na.rm = TRUE), myStd = myStd(x),
    var=var(x,na.rm = TRUE), myVar = myVar(x))
    }
  else{
    c(min = NA, myMin =NA,
      max = NA, myMax= NA,
      mean = NA, myMean=NA,
      median = NA, myMedain = NA,
      std = NA, myStd = NA,
      var=NA, myVar =NA) 
  
    }
  }

sumStat(data$rna_act_3utr)
sumStat(data$rna_res_trans)
#stastical summary of data
summary<- sapply(data, sumStat)
summary
#writing to csv file
write.csv(rbind(data,summary), file="summaryDefault.csv")

