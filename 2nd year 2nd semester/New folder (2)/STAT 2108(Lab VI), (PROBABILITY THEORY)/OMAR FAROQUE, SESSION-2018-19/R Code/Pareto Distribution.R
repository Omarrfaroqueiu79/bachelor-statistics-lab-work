# Pareto Distribution PDF and CDF
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

# Pareto Distribution PDF

PdfParetodist=function(x,a,k)
{
  fx=((a*(k)^a)/x^(a+1)) 
  return(fx)
}

curve(PdfParetodist(x,1,1),xlim = c(0,10),ylim=c(0,5),col="blue",lwd=2,main="Pareto Distribution PDF",xlab = "x",ylab = "f(x)")
curve(PdfParetodist(x,1,2),from = 0,to=10,col="black",lwd=2,add=T)
curve(PdfParetodist(x,1,3),from = 0,to=10,col="green",lwd=2,add=T)


legend(x="topright",legend=c("a=1","a=2","a=3"),col=c("blue","black","green"),
       lty = 1,cex = 1)


# Pareto Distribution CDF

CdfParetodist=function(x,a,k)
{
  FX=(1-(k/x)^a)
  return(FX)
}

      
#Pareto Distribution CDF  

CdfParetodist=function(x,a,k){
  FX=(1-(k/x)^a)
  return(FX)
}

curve(CdfParetodist(x,1,1),xlim = c(0,10),ylim=c(0,0.9),col="blue",lwd=2,main="Pareto Distribution CDF"
      ,xlab = "x",ylab = "F(x)")

curve(CdfParetodist(x,1,2),from = 0,to=10,col="black",lwd=2,add=T)
curve(CdfParetodist(x,1,3),from = 0,to=10,col="green",lwd=2,add=T)

legend(x="bottomright",legend=c("a=1","a=2","a=3"),col=c("blue","black","green"),
       lty = 1,cex = 0.75)




