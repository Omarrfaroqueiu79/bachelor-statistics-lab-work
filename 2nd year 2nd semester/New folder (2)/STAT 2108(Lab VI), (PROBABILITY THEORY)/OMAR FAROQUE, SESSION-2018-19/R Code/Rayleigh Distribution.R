# Rayleigh Distribution CDF and CDF
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

# Rayleingh Distribution PDF

PdfRaydist=function(x,a){
  fx=(2*x/a^2)*exp(-(x^2/a^2))
  return(fx)
  
}

curve(PdfRaydist(x,0.5),xlim=c(0,6),ylim=c(0,1.9),col="blue",ylab="f(x)",main="Rayleigh Distribution PDF",lwd=2)
curve(PdfRaydist(x,1),from=0,to=5,col="black",lwd=2,add=T)
curve(PdfRaydist(x,2),from=0,to=5,col="green",lwd=2,add=T)
curve(PdfRaydist(x,3),from=0,to=5,col="red",lwd=2,add=T)
curve(PdfRaydist(x,4),from=0,to=5,col="pink",lwd=2,add=T)

legend(x="topright",legend=c("a = 0.5","a = 1","a = 2","a = 3","a = 4"),
       col=c("blue","black","green","red","pink"), lty=1,cex=0.9)



# Rayleingh Distribution CDF

CdfRaydist=function(x,a){
  Fx=(1-exp(-x^2/a^2))
  return(Fx)
  
}
curve(CdfRaydist(x,0.5),xlim=c(0,8),col="blue",ylab="F(X)",
      main="Rayleigh Distribution CDF",lwd=2)

curve(CdfRaydist(x,1),from=0,to=9,col="black",lwd=2,add=T)
curve(CdfRaydist(x,2),from=0,to=9,col="green",lwd=2,add=T)
curve(CdfRaydist(x,3),from=0,to=9,col="red",lwd=2,add=T)
curve(CdfRaydist(x,4),from=0,to=9,col="pink",lwd=2,add=T)

legend(x="bottomright",legend=c("a = 0.5","a = 1","a = 2","a = 3","a = 4"),
      col=c("blue","black","green","red","pink"), lty=1,cex=0.8)


