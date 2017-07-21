#install.packages("StatMatch")
require(StatMatch)
require(RWeka)

data <- read.csv("macdonalAnalysis.csv") # read refined data file
head(data)

data <- data[, which(!grepl("^review$", colnames(data)))] # remove review
head(data)

data <- data[, which(!grepl("X", colnames(data)))] # remove X

head(data)
colnames(data)

data <- data[, which(!grepl("Time", colnames(data)))] # remove time 
head(data)

cl1 <- SimpleKMeans(data,Weka_control(N = 6))
cl1
