


#descritption: formating sql output file 
#author narendra 


data <- read.csv("outPutDataq.csv", header=TRUE)
head(data)


data[data$day==1, data$hour==23,]

day <- unique(data$day)
hour <- unique(data$hour)
users <- unique(data$userId)
use <- unique(data$tertiaryCategory)

hourData <- subset(data,data$day==1& data$hour==23)
head(hourData)


write.csv(hourData,file="hourdata.csv")

