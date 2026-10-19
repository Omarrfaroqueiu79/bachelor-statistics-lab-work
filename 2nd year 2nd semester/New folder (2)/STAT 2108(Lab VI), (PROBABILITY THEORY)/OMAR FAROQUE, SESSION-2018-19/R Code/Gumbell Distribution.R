# Gumbell Distribution PDF and CDF#
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

# Gumbell Distributon PDF

pdfGum = function(x,a,b){
  
  fx = (1/b)*exp(-( (x-a/b)+ exp(-(x-a/b) ) ) )
  return(fx)
}

curve(pdfGum(x,0.5,2), ylab="f(x)",xlim=c(-5,15),ylim=c(0,0.22),col="blue",
      lwd=2,main="Gumbell Distribution PDF")

curve(pdfGum(x,1,3),from=-5,to = 15,lwd=2,col="black",add=T)
curve(pdfGum(x,1.5,3),from= -5,to = 15,lwd=2,col="red",add=T)
curve(pdfGum(x,3,4),from= -5,to = 15,lwd=2,col="orange",add=T)

legend(x="topright", legend= c( "a= 0.5,b = 2","a=1,b = 3", "a = 1.5,b = 3", "a = 3,b = 4"),
       col =c( "blue","black","red","orange"),lty=1,cex=0.6)



# Gumbell Distributon CDF

cdfGum = function(x,a,b){
  
  Fx = (exp(-exp(-(x-a/b)) ))
  return(Fx)
}

curve(cdfGum(x,0.5,3), ylab="F(x)",xlim=c(-2,8),ylim=c(0,1),col="blue",
      lwd=2,main="Gumbell Distribution CDF")

curve(cdfGum(x,1,3),from=-5,to = 20,lwd=2,col="black",add=T)
curve(cdfGum(x,1.5,3),from= -5,to = 20,lwd=2,col="red",add=T)
curve(cdfGum(x,3,4),from= -5,to = 20,lwd=2,col="orange",add=T)

legend(x="bottomright", legend= c( "a = 0.5,b = 2","a = 1,b = 3", "a = 1.5,b = 3", "a = 3,b = 4"),
      col =c( "blue","black","red","orange"),lty=1,cex=0.7)



