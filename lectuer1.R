tab<-read.csv("msleep_ggplot2.csv")
class(tab)
head(tab)
dim(tab)
c(tab$sleep_total,1000)
summary(plot(tab$brainwt,tab$sleep_total,log="x"))
summary(tab$brainwt)
summary(tab$sleep_total)
tab[c(1,2),]
 hello<-tab[tab$sleep_total>18,]
tab$sleep_total[c(1,2)]
head(tab)
hello
mean(hello$sleep_tota)
which(tab$sleep_total>18 & tab$sleep_rem<3)
tab$sleep_total[which(tab$sleep_total>18)[1]]
sort(tab$sleep_total)
order(tab$sleep_total)
rank(tab$sleep_total)
idx <-match(c("Cow","Owl monkey","Cheetah","Cotton rat"),tab$name)
tab[idx,]
fac<- factor(tab$order)
fac
levels(fac)
table(tab$order =="Rodentia")
tab
head(tab)
s =split(tab$sleep_total,tab$order)
s
mean(s[["Rodentia"]])
lapply(s,mean)
sapply(s,mean)
tapply(tab$sleep_total,tab$order,mean)
sd(s[["Primates"]])
