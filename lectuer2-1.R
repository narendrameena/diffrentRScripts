dat <- read.csv("femaleMiceWeights.csv")
diff<-mean(dat[13:24,2])-mean(dat[1:12,2])
s = split(dat[,2],dat[,1])
s
stripchart(s,vertical=TRUE,col=1:2)
abline(h=sapply(s,mean),col=1:2)
h<-dat[13:24,2]<mean(dat[1:12,2])
h
f<-dat[1:12,2]<mean(dat[13:24,2])
sum(f ==FALSE)
which(f)
highfat=s[["hf"]]
highfat
sample(highfat,6)
sample(highfat,6,replace=TRUE)
sum(highfat > 30)/length(highfat)

population <- read.csv("femaleControlsPopulation.csv")
population = population[,1]
mean(population)
sample(poupulation,12)
mean(sample(population,12))
sampleMean = replicate(10000,mean(sample(population,12)))
head(sampleMean)
plot(sampleMean)
null = replicate(10000,mean(sample(population,12))-mean(mean(sample(population,12))))
head(null)
plot(null)
hist(null)
abline(v=diff,col="red")
abline(v=-diff,col="red")
diff
summary(null)
pnorm(null)
