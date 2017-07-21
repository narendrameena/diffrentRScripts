

# author@narendra
# info@summary statistics of test data 

#read delimited file of test data
library(pastecs)
data <- read.delim("test1.txt",header = TRUE)
#method 1
#using default summary function
write.csv(summary(data), file="summaryDefault.csv")
#method 2
#using pastecs package's summary function
write.csv(stat.desc(data), file="summaryPastecs.csv") 



