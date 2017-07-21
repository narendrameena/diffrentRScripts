set.seed(1)
dat <- read.csv("mice_pheno.csv")
head(dat)
hfpop <- dat[dat$Sex=="F" & dat$Diet=="hf",3]
chowpop <- dat[dat$sex=="M" & dat$Diet=="chow",3]
chowpop
hfpop
dat
N<-5
hf <- sample(hfpop,N)
chow <- sample(chowpop,5)
t.test(hf,chow)

N <-12
alpha <- 0.05
B <- 10000

rejections<- sapply(1:B function(i){
  hf <- sample(hfpop,N)
  chow <- sample(chowpop,N )
  t.test(hf,chow)$p.value < alpha
})

head(rejections)
mean(rejections)
