# Laplace Distribution PDF and CDF
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

# Laplace Distribution CDF

PdfLaplacedist=function(x,a,l){
  
  fx=(1/2*l)*exp(-(abs(x-a)/l))
  return(fx)
}



curve(PdfLaplacedist(x,0,1),xlim = c(-20,20),ylim=c(0,2),col="blue",
      main="Laplace Distribution PDF",xlab = "x",ylab = "f(x)")

curve(PdfLaplacedist(x,0,2),from = -20,to=20,col="black",lwd=2,add=T)
curve(PdfLaplacedist(x,0,4),from = -20,to=20,col="green",lwd=2,add=T)
curve(PdfLaplacedist(x,-5,4),from = -20,to=20,col="yellow",lwd=2,add=T)

legend(x="topright",legend=c("a=1","a=2","a=4","a=4"),
       col=c("blue","black","green","yellow"),lwd=2,cex=0.7)
      
      
      
      
#Laplace Distribution CDF  

CdfLaplacedist=function(x,a,l){
  FX=(1-(1/2)*exp(-((x-a)/l)))
  return(FX)
}

curve(CdfLaplacedist(x,0,1),xlim = c(-10,8),ylim=c(0,1),
      col="blue",main="Laplace Distribution CDF",xlab = "x",ylab = "F(x)",lwd=2)

curve(CdfLaplacedist(x,0,2),from = -10,to=8,col="black",lwd=2,add=T)
curve(CdfLaplacedist(x,0,4),from = -10,to=8,col="green",lwd=2,add=T)
curve(CdfLaplacedist(x,-5,4),from = -10,to=8,col="yellow",lwd=2,add=T)

legend(x="bottomright",legend=c("a=1","a=2","a=4","a=4"),
       col=c("blue","black","green","yellow"),lwd=2,cex=0.7)



