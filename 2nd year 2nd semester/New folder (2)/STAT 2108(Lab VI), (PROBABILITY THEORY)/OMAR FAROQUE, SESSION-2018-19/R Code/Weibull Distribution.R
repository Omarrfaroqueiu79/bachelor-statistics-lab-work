# Weibull Distribution PDF and CDF
# parameter data from https://www.wikipedia.org/

par(mfrow=c(1,2))

##########################################
#### The extreme wind speed Data Set  ####
##########################################



x= c( 24.52, 69.15, 33.21, 30.79, 18.12, 94.34, 55.40, 28.28,
      49.73, 15.92, 21.81, 21.80, 30.56, 18.56, 87.63, 40.53,
      43.31, 20.10, 27.56, 90.16, 57.14, 28.98, 43.03, 49.13,
      50.47, 31.09, 27.17, 20.59, 65.99, 18.05, 19.44, 54.70,
      31.34, 48.32, 30.11, 46.22, 40.60, 24.76, 46.64, 75.50,
      45.78, 26.14, 23.06, 33.01, 51.56, 55.22, 67.04, 34.59,
      24.32, 55.88, 31.74, 76.17)


# Weibull Distribution PDF

dWeibull=function(x,b,k){
  
  fx=((k/b)*(x/b)^(k-1))*(exp(-(x/b)^k))
  return(fx)
  
}

curve(dweibull(x,1,0.5), from=0, to=10, main = 'Weibull Distribution PDF',
      ylab = ' f(x)', lwd = 2,ylim=c(0,.8),
      col = 'pink',type="l")
curve(dweibull(x,1,1), from=0, to=5, col='blue', add=TRUE ,lwd = 2)
curve(dweibull(x,1,1.5), from=0, to=8, col='red', add=TRUE, lwd = 2)
curve(dweibull(x,3,4), from=0, to=8, col='black', add=TRUE ,lwd = 2)

legend(x="topright",legend=c("b=1,k=0.5","b=1,k=1","b=1,k=1.5","b=3,k=4"),
       col=c("pink","blue","red","black"),lty=1, cex=0.8)


# Weibull Distribution CDF

cdf=function(x,b,k){
  
  Fx=(1-(exp(-(x/b)^(k))))
  return(Fx)
  
}


curve(cdf(x,1,0.5), from=0, to=5, main = 'Weibull Distribution CDF',
      ylab = 'F(x)', lwd = 2,ylim=c(0,1),
      col = 'pink')
curve(cdf(x,1,1), from=0, to=5, col='blue', add=TRUE ,lwd = 2)
curve(cdf(x,1,1.5), from=0, to=5, col='red', add=TRUE, lwd = 2)
curve(cdf(x,3,4), from=0, to=5, col='black', add=TRUE, lwd = 2)

legend(x="bottomright",legend=c("b=1,k=0.5","b=1,k=1","b=1,k=1.5","b=3,k=4"),
       col=c("pink","blue","red","black"),lty=1, cex=.55)

