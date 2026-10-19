#16
library("datasets")
data<-iris
head(data)
#ASCII
write.table(data, file = "D:/a.asc")
#stata
library(haven)
write_dta(iris, path="D:/s.dta")
#spss
library(haven)
write_sav(data=iris, path = "D:/ss.sav")
#excel
write.csv(iris, file = "D:/e.csv")
#xlsx
install.packages('writexl')
library("writexl")
write_xlsx(iris,path ="D:/x.xlsx")
#Text
write.table(data, file = "D:/t.txt") 
?dgamma

dgamma(x, shape, rate = 1, scale = 1/rate, log = FALSE)
pgamma(q, shape, rate = 1, scale = 1/rate, lower.tail = TRUE,
       log.p = FALSE)
qgamma(p, shape, rate = 1, scale = 1/rate, lower.tail = TRUE,
       log.p = FALSE)
rgamma(n, shape, rate = 1, scale = 1/rate)

###Arguments
#x, q	
#vector of quantiles.
#p	
#vector of probabilities.
#n	
#number of observations. If length(n) > 1, the length is taken to be the number required.
#rate	
#an alternative way to specify the scale.
#shape, scale	
#shape and scale parameters. Must be positive, scale strictly.
#log, log.p	
#logical; if TRUE, probabilities/densities p are returned as log(p).
#lower.tail	
#logical; if TRUE (default), probabilities are P[X ??? x], otherwise, P[X > x].

?dunif
dunif(5,0,10)
punif(6,0,10, lower.tail = FALSE)
qunif(.9, 0,10)
r<-runif(100,0,10)

#extra
plot(density(r))

x<-c(2,3,4,5,6)
y<-c(4,5,6,7,8)
cor(x,y)
model<-lm(y~x)
summary(model)

#correlation between x and y
x<-c(2,3,4,5,6)
y<-c(4,5,6,7,8)
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
cf(data$x,data$y)


#pass/fail
x<-c(45,30,60,68.75,35,64,45,25,29,50,52,26,80,65,25)
x
seq(x)
for (i in seq(x)) {
        if(x[i]<50){
                print("failed")
        }
        else{
                print("passed")
        }   
}
#product of series
#2/1,3/2,......10/9

y=1
for (i in 1:9) {
        y=y*((i+1)/i)
        y
}
y


#2^2/1,3^2/2,............N^2/N-1

p<-function(n){
        y=1
        for (i in 2:n) {
                y=y*((i^2)/(i-1))
        }
        return(y)
}
p(3)


#smoking vs hypertension
s<-c(60,40)
n<-c(20,30)
df<-data.frame(s,n)
df
#association test
a<-chisq.test(df)
a
#observed and expected values
names(a)
a$observed
a$expected
#plotting residuals
plot(a$residuals)
?names
?prop.test
prop.test(13173,25468,.5, alternative = "greater")
 