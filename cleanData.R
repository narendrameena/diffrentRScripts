#author narumeena
#description remove rows that contain NA from the data in attribute time
#date 15/12/2015




data <- read.csv("dtm.csv")

data <- data[iris$Date !="NA",] # excluse row ehre date is NA

write.csv(data,file="cleanData.csv")