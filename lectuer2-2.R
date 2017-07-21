library(devtools)
install_github("jennybc/gapminder")
library(gapminder)
data(gapminder)
head(gapminder)
s =gapminder$year[[1952]]
table(gapminder$year ==1952)
hist(gapminder$lifeExp[which(gapminder$year==1952)])
hist(log10(gapminder$lifeExp[which(gapminder$year==1952)]))
mean(gapminder$lifeExp[which(gapminder$year==1952)] <=40)
mean(gapminder$lifeExp[which(gapminder$year==1952)]<=60 )-mean( gapminder$lifeExp[which(gapminder$year==1952)] <=40) 

prop =function(q){
  mean(50<=q)
}
qs =seq(from=min(1),to=max(100),length=20)
props =sapply(qs,prop)
plot(qs,props)
props =sapply(qs,function(q) mean(30<=q))
plot(ecdf)
pop = gapminder$pop[which(gapminder$year==1952)]
s = split(gapminder$pop[which(gapminder$year==1952)],gapminder$country[which(gapminder$year==1952)])
s
s<- plot(gapminder$country[which(gapminder$year==1952)],log10(gapminder$pop[which(gapminder$year==1952)]), type="h")
s
hist(log10(gapminder$pop[which(gapminder$year==1952)]))
sd(log10(gapminder$pop[which(gapminder$year==1952)]))
mean(log10(gapminder$pop[which(gapminder$year==1952)]))
fun <- function(q){
  return(pnorm(q,mean=mean(log10(gapminder$pop[which(gapminder$year==1952)])),sd=sd(log10(gapminder$pop[which(gapminder$year==1952)]))))
}
nd<-fun(7)-fun(6)
nd
n = length(log10(gapminder$pop[which(gapminder$year==1952)]))
n
nc=n*nd
nc
qqnorm(log10(gapminder$pop[which(gapminder$year==1952)]))
ps=((1:n)-0.5)/n
ps
qqnorm(ps)
sort(log10(gapminder$pop[which(gapminder$year==1952)]))
plot(qnorm(ps),sort(log10(gapminder$pop[which(gapminder$year==1952)])))
