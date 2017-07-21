options(download.file.method="wget")

library(devtools)

dat<- read.csv("femaleMiceWeights.csv")
dat[1:12,2]
mean(dat[13:24,2])-mean(dat[1:12,2])
pop<-read.csv("femaleControlsPopulation.csv")
pop

control<-sample(pop[,1],12)
mean(control)

n<-10000
null<-vector("numeric",n)
for(i in 1:n){
  controle <- sample(pop[,1],12)
  treatment <- sample(pop[,1],12)
  null[i] <- mean(treatment)- mean(control)
}

diff <-mean(dat[13:24,2])-mean(dat[1:12,2])
null>diff
mean(null>diff)

hist(nill)
qqnorm(null)
qqline(null)



n<-100
library(rafalib)
mypar(1,1)
plot(0,0,xlim=c(-5,5),ylim=c(1,30),type="n")
totals <- vector("numeric",11)
for(i in 1:n){
  controle <- sample(pop[,1],12)
  treatment <- sample(pop[,1],12)
  nulldiff <- mean(treatment)- mean(control)
  j <- pmax(pmin(round(nulldiff)+6,11),1)
  totals[j]<-totals[j]+1
  text(j-6,totals[j],pch=15,round(nulldiff,1),cex=0.75)
  if(i<15)scan()
  
}
