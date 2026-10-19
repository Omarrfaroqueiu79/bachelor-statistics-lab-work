#       S-1
X<-matrix(c(5,9,7,4,3,8,6,7,5),nrow = 3, byrow = T)
print(X)
Y<-matrix(c(7,9,4,8,5,3,5,2,6),nrow=3, byrow = T)
print(Y)
#inverse of X
solve(X)
#determinant of X
det(x)
#trace of x
sum(diag(X))
#calculating eigen values and vectors
e<-eigen(X)
e$values
e$vectors
#matrix multiplication
print(X%*%Y)
#product of x and y(element wise)
print(X*Y)


#create a vector x
x<-c(10.4,5.6,3.1,6.4,27.7,6.5)
mean(x)
var(x)
sd(x)
sort(x,decreasing = F)
#Obtain diagonal value
diag(X)

#construct a vector p
p<-c(4,5,6,7,5,6,9,4,3)
mean(p)
median(p)

#In R, the package DescTools includes the Mode() function.
#First, we have to import the library, together with the standard ones:
#import libraries
install.packages("DescTools")
library(readr)
library(dplyr)
library(DescTools)
m<-Mode(p)
m
#A set of numbers with two modes is bimodal, 
#a set of numbers with three modes is trimodal, 
#and any set of numbers with more than one mode is multimodal.
#variance
var(p)
#standard deviation
sd(p)
#quantiles
q1<-quantile(p,0.25)
q2<-quantile(p,0.5)
q3<-quantile(p,0.75)
q4<-quantile(p, 1)
q1
q2
q3
q4
#interquartile range
IQR(p)
q3-q1
#p in descending order
sort(p, decreasing=F)
X<-matrix(p,nrow=3,ncol=3)
#manipulating rows
r1<-X[1,]
r1
r2<-X[2,]
r3<-X[3,]
r1
r2
r3
#manipulating columns
c1<-X[,1]
c2<-X[,2]
c3<-X[,3]
c1
c2
c3

#From suggestion
#correlation of x,y
cor(X,Y)
# This function returns a correlation matrix

#To find the correlation between corresponding columns of two matrices,
#we can use mapply function but we will have to read the matrices
#using as.data.frame function.
c<-mapply(cor,as.data.frame(X), as.data.frame(Y))
#regression
model<-lm(Y~X)
model
b<-t((X)%*%X)-(t(X)%*%Y)
b
#end of suggestion part

#generating sequence
#and repeating
seq(-10,10, by=0.05)
rep(seq(-5,5,0.2),5)
#calculating sum of square of a sequence using for loop
x<-seq(1:100)
x
y=0
for (i in x) {
      y=y+x[i]^2
      y
}
y
#calculating product of the series using for loop
#1)     2/1,3/2,4/3,,,,,,,,,,,,,10/9
p=1
for (i in 1:9) {
        p=p*((i+1)/i)
        p
}
p
#2)      2^2/1, 3^2/2,............,n^2/n-1

prd<-function(n){
        p=1
        for (i in 2:n) {
                p=p*((i^2)/(i-1))
        }
        return(p)
}

prd(3)

#user defined function to find correlation
cf<-function(x,y){
        sxy=sum(x*y)
        sxx=sum(x*x)
        syy=sum(y*y)
        xbar=mean(x)
        ybar=mean(y)
        n=length(x)
        a=sxy-n*xbar*ybar
        b=sxx-(n*xbar^2)
        c=syy-(n*ybar^2)
        r=a/sqrt(b*c)
        print(r)
}
x<-c(2,4,4,8)
y<-c(1,7,5,7)
cor(x,y)
cf(x,y)
#user defined function to find covariance
cv<-function(x,y){
        sxy=sum(x*y)
        xbar=mean(x)
        ybar=mean(y)
        n=length(x)
        a=sxy-n*xbar*ybar
        c=a/(n-1)
        print(c)
}
cv(x,y)
cov(x,y)

#15
x<-seq(-10,10,by=.05)
x
help('dnorm')
#Normal distribution
#X~N(5,25)
dnorm(15,5,5)
pnorm(2,5,5)
qnorm(.76,5,5)

#X~N(5,.40)
dnorm(4,5,sqrt(0.40))
pnorm(2,5,sqrt(0.40),lower.tail = FALSE)
pnorm(3,5,sqrt(0.40))
qnorm(.87,5,sqrt(.40))

p<-rnorm(100000,5,sqrt(.40))
hist(p)
boxplot(p)
plot(density(p))

?dnorm

?dchisq
?dpois

#tax accountants, income, mean state-wise

incomes<-seq(50,79,by=1)
incomes
statef<-factor(c('tas','nt', 'qld','nsw', 'nt',
                 'tas','nt', 'qld','nsw', 'nt',
                 'tas','nt', 'qld','nsw', 'nt',
                 'tas','nt', 'qld','nsw', 'nt',
                 'tas','nt', 'qld','nsw', 'nt',
                 'tas','nt', 'qld','nsw', 'nt'))
nlevels(statef)
t<-table(incomes,statef)
t
class(t)
incmeans<-tapply(incomes, statef, mean)
incmeans
#form matrix from income
m<-matrix(incomes,nrow=5,ncol = 6, byrow = T)
m
#remove 6th column
m<-m[,-6]
m
#replace the first element
m[1,1]<-58
m

#weibull
#plotting density function
curve(dweibull(x, 1, 1) ,from = 0, to= 4,lty=1,
      main = "Density function of weibull dist.",
      xlab = 'X',
      ylab = 'f(x)'
      )
curve(dweibull(x, 1.5, 1), from = 0, to= 4, add = T, col='red', lty=2)
curve(dweibull(x,3,1), from = 0, to= 4, add = T, col='blue', lty=3)

legend(x='topright',
       legend = c('a', 'b', 'c'),
       lty = c(1,2,3),
       col = c('black', 'red', 'blue'),
)


#plotting hazard function
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#input data and compute cumulative hazard
time<-c(37,NA, 73, NA,132,195, NA, 222, 248, NA)
fail<-c(1,0,1,0,1,1,0,1,1,0)
revrank<-c(length(fail):1)
revrank
haz=fail/revrank
haz
cumhaz=cumsum(haz)
cumhaz

#selecting failing cases for plotting
df<-data.frame(time,fail,cumhaz,haz)
df
z<-subset(df, fail==1)
z


plot(z$time, z$haz, type = 'o', pch=19, col='blue',
     xlab='time',
     ylab = 'cumulative hazard',
     main='weibull hazard'
     )
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


install.packages('survival')
library(survival)
install.packages('eha')
library(eha)
hweibull(3,2,1)
curve(hweibull(x,1,1),from = 0, to =4, lty=1,
      main='Weibull hazard function',
      xlab = 'x',
      ylab = 'f(x)'
)
curve(hweibull(x,1.5,1), from = 0, to =4, lty=2, col='red', add = T)
curve(hweibull(x,2,1), from = 0, to = 4, lty=3, col='blue', add=T)
legend(
  x='topright',
  legend = c('a','b','c'),
  lty=c(1,2,3),
  col=c('black', 'red', 'blue'),
  bty = ''
)



a<-read_dta("D:/waste.dta")
a
View(a)
summary(a)
w<-read.csv("D:/waste.csv")
summary(w)
cor(w)
plot(w)

#fit a model
attach(w)
lm(energy~plastic+paper+food+water)
#interaction term
reg<-lm(energy~(plastic+paper+food+water)^2)
reg
model1<-lm(energy~plastic*paper)
model1
model2<-lm(energy~plastic*food)
model2
#alternative way
model2n<-lm(energy~(paper+food+paper:food))
model2n
model3<-lm(energy~plastic*water)
model3
model4<-lm(energy~paper*food)
model4
model5<-lm(energy~paper*water)
model5
model6<-lm(energy~food*water)
model6

#selecting optimal regression
x<-aov(energy~.^2,w)
x
summary(x)
x<-aov(energy~(plastic+paper+food+water)^2)
summary(x)

c<-lm(energy~(plastic+paper+food+water))
c
AIC(c)
AIC(x)
x1<-update(x,.~.-plastic:paper,w)
x1
AIC(x1)
x2<-update(x1,.~.-plastic:food,w)
AIC(x2)
x3<-update(x2,.~.-paper:food,w)
AIC(x3)
x4<-update(x3,.~.-paper:food,w)
AIC(x4)
x5<-update(x3,.~.-paper)
AIC(x5)
x6<-update(x5,.~.-food)
AIC(x6)

summary.lm(x6)

#check residuals are normally plotted or not?
p<-resid(x6)
p

p1<-resid(x)
p
par(mfrow=c(1,2))

qqnorm(p, main = 'normal Q_Q plot for reduced model', 
       xlab = 'quantiles of standard normal',
       ylab = 'residuals')
qqline(p,lwd=2)

qqnorm(p1, main = "normal Q-Q plot for full model",
       xlab = "Quantiles of standard normal",
       ylab = "residuals")
qqline(p1, lwd=2)


#leverage value
a<-lm(energy~(plastic+paper+food+water))
a
x<-resid(a)
x
sort(x)
summary(x)

h<-lm.influence(a)$hat
h
sort(h)
plot(h)
lines(h)
summary(h)

#cooks distance
h2<-cooks.distance(a)
h2
plot(h2)
lines(h2)
sort(h2)
summary(h2)




#X~N(5,15)
pnorm(15,5,sqrt(15),lower.tail = FALSE)
pnorm(12, 5, sqrt(15))
sum(dnorm(6:12,5,sqrt(15)))
plot(density(rnorm(20,5,sqrt(15))))






