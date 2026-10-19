
#problem 3: Draw a random sample of size n=500 from chi-squre distribution with degrees of freedom 5 
# and plot .Hence over plot another two samples of same size with degrees of freedom
#are 10 and 15 respectively . comments on your results

set.seed(1)
par(mfrow = c(1,3))

x<-rchisq(500,df=5)
hist(x,breaks = "scott",freq = FALSE,xlim = c(0,20),ylim = c(0,.4),xlab = "chi squre distribution",main = "chi distribution for 5df")
curve(dchisq(x,5),from = 0, to = 20,n=500,col="black",lwd = 2, add = T)

x<-rchisq(500,df=10)
hist(x,breaks = "scott",freq = FALSE,xlim = c(0,20),ylim = c(0,.4),xlab = "chi squre distribution",main = "chi distribution for 10df")
curve(dchisq(x,10),from = 0, to = 20,n=500,col="blue",lwd = 2, add = T)

x<-rchisq(500,df=15)
hist(x,breaks = "scott",freq = FALSE,xlim = c(0,25),ylim = c(0,.4),xlab = "chi squre distribution",main = "chi distribution for 15df")
curve(dchisq(x,15),from = 0, to = 20,n=500,col="green",lwd = 2, add = T)