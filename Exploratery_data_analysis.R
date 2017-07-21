###......................
install.packages("UsingR")


library(UsingR)
x = father.son$fheight
head(x)
length(x)
##.........

round(sample(x,20),1)
##....

mean(x>59)
1-pnorm(59,mean(x),sd(x))
###.........
ps <- seq(0.01,0.99,0.01)

qs <- quantile(x,ps)
normalqs <- qnorm(ps,mean(x),sd(x))
plot(normalqs,qs,xlab="Normal Pecentile", ylab="height")
abline(0,1)

##.....
qqnorm(x)
qqline(x)

##........




##.......
hist(x,breaks=seq(floor(min(x)),ceiling(max(x))),main ="Height histogram",Xlab="Height in inches")

##.......

xs<- seq(floor(min(x)),ceiling(max(x)),0.1)
xs
plot(xs,ecdf(x)(xs),type="l",xlab="Height in inches", ylab="F(x)")
abline(0,1)  # identify line



#####........

load("skew.RData")

dim(dat)
head(dat)
par(mfrow=c(3,3))
sapply(1:9,function(i){qqnorm(dat[,i])})
sapply(1:9,function(i){hist(dat[,i])})
d <- sapply(1:9,function(i){qnorm(dat[,i])})
head(d)
mean(dat[1])
dat[1,1]


par(mfrow=c(1,1))

###.......

hist(exec.pay)
qqnorm(exec.pay)
qqline(exec.pay)


#####........

boxplot(exec.pay,ylab="100000 dollers",ylim = c(0,400))
median(exec.pay)

InsectSprays
head(InsectSprays)
dim(InsectSprays)
boxplot(split(values,factor))


####.........

data("father.son")
x=father.son$fheight
y=father.son$sheight

plot(x,y,xlab="Father's heights in inches", ylab="Son's heights heighta in inches")
plot(x,y,xlab="Father's heights in inches", ylab="Son's heights heighta in inches",main=paste("correlation =",signif(cor(x,y),2)))


#####......,fig.width=6, fig.height=3
boxplot(split(y,round(x)))
print(mean(y[round(x)==72]))


####.............

x=(x-mean(x))/sd(x)
y=(y-mean(y))/sd(y)
length(x)
length(y)
means = tapply(y,round(x*4)/4,mean)
fatherheights=as.numeric(names(means))
plot(fatherheights,means,ylab="avrage of starta of son heights", ylim=range(fatherheights))
abline(0,cor(x,y))


####..........

install.packages("UsingR")
library(UsingR)
data(father.son)

plot(father.son$fheight,father.son$sheight)
cor(father.son$fheight,father.son$sheight)
identify(father.son$fheight,father.son$sheight)

x =father.son$fheight
y= father.son$sheight
n =nrow(father.son)

plot(scale(x),scale(y))
abline(h=0,v=0)

cor(x,y)
sum(scale(x)*scale(y))/(n-1)


#####.......

library(UsingR)
data(nym.2002)

head(nym.2002)

# histogram of times
hist(nym.2002$time)

# plot of runner vs their time
plot(nym.2002$age,nym.2002$time)


# plot of runner's time vs their place in the race

plot(nym.2002$time,nym.2002$place)

# qqnorm of times

qqnorm(nym.2002$time)

boxplot(tail(sort(table(nym.2002$home)),10))

boxplot(split(nym.2002$age,nym.2002$gender))

boxplot(split(nym.2002$age,nym.2002$home))


time = sort(nym.2002$time)
time
median(time)


