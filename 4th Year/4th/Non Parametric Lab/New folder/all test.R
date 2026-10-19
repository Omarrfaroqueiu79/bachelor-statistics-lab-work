#Shapiro-Wilk test 
x<-read.csv()
x
str(x)
shapiro.test(x$)
#Kolmogorov-Smirnov Test 
y<-rnorm(150)
y
ks.test(y,"pnorm")
#Kolmogorov-Smirnov Test 
head(women)
str(women)
#graps
plot(women$height)
plot(density(women$height))
#null hypothesis is normaliy distributed
shapiro.test(women$height)
ks.test(women$height,"pnorm")
#sign test
library("BSDA")
h<-read.table()
attach(h)
names(h)
difference<-h$height-h$weight
qqline(difference)
SIGN.test(difference,md=0,alternative="two sided",conf.level=0.95)

