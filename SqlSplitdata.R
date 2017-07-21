#author narumeena
#date 02/02/2016

#description: aplititng some miaaraged data and putting it back into understable format.  

install.packages("dplyr")
library(dplyr)


data <-read.csv("Data/orders062011.csv")

head(data)
data$revisedScreenSize <-data$revisedScreenSize.split
data$age

install.packages("data.table")
library(data.table)
