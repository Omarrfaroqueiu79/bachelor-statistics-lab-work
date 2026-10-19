# Logistic Distribution CDF and PDF
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

# Logistics Distribution PDF

pdfLogis=function(x,a,b){
  
  fx=(((1/b)*(exp(-(x-a)/b)))/((1+exp(-(x-a)/b))^2))
  return(fx)
  
}

curve(pdfLogis(x,5,2),ylab = "f(x)",main= " Logistic Distribution PDF",
      xlim= c(-5,25),ylim= c(0,0.3), col ="blue",lwd=2)
curve(pdfLogis(x,9,3),from = -5,to= 25,col="black",lwd=2,add=T)
curve(pdfLogis(x,9,4),from = -5,to= 25,col =" red",lwd=2 ,add=T)
curve(pdfLogis(x,6,2),from = -5,to= 25,col="green",lwd= 2,add=T)
curve(pdfLogis(x,2,1),from = -5,to= 25,col="burlywood4",lwd= 2,add=T)

legend(x="topright",legend=c("a=5,b=2","a=9,b=3","a=9,b=4","a=6,b=2","a=2,b=1"),
       col=c("blue","black","red","green","burlywood4"),lty=1, cex=0.7)



# Logistics Distribution CDF

cdfLongis=function(x,a,b){

  Fx=(1)/(1+exp(-(x-a)/b))
  return(Fx)
}

curve(cdfLongis(x,5,2),ylab = "F(x)",main= " Logistic Distribution CDF",
      xlim= c(-5,20),ylim= c(0,1), col ="blue",lwd=2)

curve(cdfLongis(x,9,3),from = -5,to= 20,col="black",lwd=2,add=T)
curve(cdfLongis(x,9,4),from = -5,to= 20,col =" red",lwd=2 ,add=T)
curve(cdfLongis(x,6,2),from = -5,to= 20,col="green",lwd= 2,add=T)
curve(cdfLongis(x,2,1),from = -5,to= 20,col="burlywood4",lwd= 2,add=T)

legend(x="bottomright",legend=c("a=5,b=2","a=9,b=3","a=9,b=4","a=6,b=2","a=2,b=1"),
       col=c("blue","black","red","green","burlywood4"),lty=1, cex=0.7)

