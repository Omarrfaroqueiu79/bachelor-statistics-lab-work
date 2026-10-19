#2017
#1-d
# obtaine those matrix
X<-matrix(c(5,9,7,4,3,8,6,7,5), nrow = 3, ncol = 3, byrow = TRUE)
X
Y<-matrix(c(7,9,4,8,5,3,5,2,6), nrow = 3, ncol = 3, byrow = TRUE)
Y
#matrix product
X%*%Y

#element by element
X*Y

#eigen value of X
eigen(X)
#eigen value of Y
eigen(Y)

#correlation using for loop

data<-data.frame(
        x<-c(5,9,4,1,4),
        y<-c(17,15,15,14,12)
)


crf<-function(x,y){
        sxy=0
        sx=0
        sy=0
        sxx=0
        syy=0
        n=length(x)
        
        for (i in 1:n) {
                sxy=sxy+x[i]*y[i]
                sx=sx+x[i]
                sy=sy+y[i]
                sxx=sxx+x[i]^2
                syy=syy+y[i]^2
        }
        
        xbar=sx/n
        ybar=sy/n
        a=sxy-n*xbar*ybar
        b=sxx-n*xbar^2
        c=syy-n*ybar^2
        r=a/sqrt(b*c)
        return(r)
        
        
}
crf(data$x,data$y)


#pass/fail
marks<-c(65,34,78,23,43,45,67,67,98,34,46,55,66,39,90)

for (i in seq(marks)) {
        if(marks[i]<45){
                print("failed")
        }
        else{
                print("passed")
        }
}


#sum of squares
x<-seq(200:1000)
x
y=0
for (i in x) {
        y=y+x[i]^2
        y
}
y

#p45


#X~Binom(100,0.5)
sum(dbinom(46:55,100,0.5))
dbinom(4,100,0.5)
#logistic fit toxicity

ldose<-rep(0:5,2)

ldose

numdead<-c(1,4,9,13,18,20,0,2,6,10,12,16)
numdead

sex<-factor(rep(c("M","F"),c(6,6)))
sex

sf<-cbind(numdead,numalive=20-numdead)
sf

budworm.lg<-glm(sf~sex*ldose, family = binomial)
summary(budworm.lg, cor=F)

budworm.lgA<-update(budworm.lg, .~sex*I(ldose-3))
summary(budworm.lgA, cor=F)$coefficients

anova(update(budworm.lg, .~.+sex*I(ldose^2)), test = "Chisq")



a<-c(60,40)
b<-c(20,30)
df<-data.frame(a,b)
df
d<-chisq.test(df)
d$expected
d$observed

#4-c
#3 type of cars
dom<-c(18,17,15,19,16)
jap<-c(20,15,15,14,19)
eur<-c(17,15,14,18,17)
df<-data.frame(dom,jap,eur)
df
b<-stack(df)
b

a<-anova(lm(values~ind,b))
a
summary(a)
#hypothesis test
o<-oneway.test(values~ind,b, var.equal = TRUE)
o

#single variance test
x<-c(5,9,7,6,8,10,15,11)
x


varTest(x,sigma.squared = 8)

install.packages("EnvStats")
library(EnvStats)
#17-4
initial.investment<-1000
rate<-4
n=3
compamount<-initial.investment*(1+(rate/100))^(0:(n-1))
compamount

comp.interest<-compamount*rate/100
comp.interest
