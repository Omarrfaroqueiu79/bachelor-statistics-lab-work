# question number 4, draw a random sample of size n=500 from 
# student t distribution with degrees of freedom 15 and plot.Hence over plot another 
# to samples of same size with degrees of freedoms 20 and 35 respectively.Comments on your results.

#student T distribution

par(mfrow = c(1,3))

set.seed(1)

n<- 500

y<-rt(n,df=15)

hist(y,breaks="scott",main= "T distibution df=15")

y<-rt(n,df=20)

hist(y,breaks="scott",main= "T distibution df=20",col="pink")

y<-rt(n,df=35)

hist(y,breaks="scott",main= "T distibution df=35",col="green")