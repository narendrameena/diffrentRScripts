
#install.packages("aod")

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)


data <- read.csv("macdonalAnalysis.csv") # read refined data file
head(data)

data <- data[, which(!grepl("^review$", colnames(data)))] # remove review
head(data)

data <- data[, which(!grepl("X", colnames(data)))] # remove X

head(data)
colnames(data)

data <- data[, which(!grepl("Time", colnames(data)))] # remove time 
head(data)

#data$policies_violated <- factor(data$policies_violated)
mylogit <-multinom(policies_violated ~  Location + Age +city, data = data) #


summary(mylogit)



z <- summary(mylogit)$coefficients/summary(mylogit)$standard.errors
z


# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


## extract the coefficients from the model and exponentiate
exp(coef(mylogit))


head(pp <- fitted(mylogit))
