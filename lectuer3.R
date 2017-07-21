babies = read.table("babies.txt",header=TRUE)
head(babies)

bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]

mean(bwt.nonsmoke)-mean(bwt.smoke)
sd(bwt.nonsmoke)
sd(bwt.smoke)


X.ns = mean(bwt.nonsmoke)
sd.ns = sd(bwt.nonsmoke)
X.s = mean(bwt.smoke)
sd.s =sd(bwt.smoke)
sd.diff =sqrt(sd.ns^2/length(bwt.nonsmoke) + sd.s^2/length(bwt.smoke))

tval =(X.ns-X.s)/sd.diff
tval

t.test(bwt.nonsmoke,bwt.smoke)


X.ns = mean(bwt.nonsmoke[1:30])
sd.ns = sd(bwt.nonsmoke[1:30])
X.s = mean(bwt.smoke[1:30])
sd.s =sd(bwt.smoke[1:30])
sd.diff =sqrt(sd.ns^2/length(bwt.nonsmoke[1:30]) + sd.s^2/length(bwt.smoke[1:30]))

tval =(X.ns-X.s)/sd.diff
tval
pval = 1-pnorm(abs(tval)) + pnorm(-abs(tval))
pval
t.test(bwt.nonsmoke[1:30],bwt.smoke[1:30])
