#problem 5
library(randtests)
data<-c(12,12,12,5,8,9,8,9,5,7,9,9,8,8,10,10)
run.test(data)
runif()


#problem 3
pimc=functon(n){
u=runif(n)
g=4*sqrt(1-u^2)
pimc=mean(n)
return(pimc)
}
pimc(10000)


#problem 2 a
punif(500,mean=10,max=20)


#problem 4
rexp=function(n,lamba){
  u=runif(n)
g=-1/lamba*log(1-u)
return(g)
}x=rexp(50,1)
sq=sort(x)
tq=function(u){-log(1-u)}
pot(sq,tq(1,50/51),xlab="sample quantiles,"y lab="theoritical quantiles")

