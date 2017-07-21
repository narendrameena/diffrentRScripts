dat <- read.csv("femaleMiceWeights.csv")
head(dat)

control <- dat[1:12,2]
treatment <- dat[12+1:12,2]
diff <- mean(treatment)- mean(control)
print(diff)

t.test(treatment,control)
sd(control)
sd(control)/sqrt(length(control))

se <- sqrt(var(treatment)/length(treatment) + var(control)/length(control))
se
tstat<-diff/se
tstat
1-pnorm(tstat)+ pnorm(-tstat)

qqnorm(control)
qqline(control)
qqnorm(treatment)
qqline(treatment)
