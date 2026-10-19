# Gumbell Distribution PDF and CDF#
# parameter data from https://www.wikipedia.org/

par(mfrow=c(1,2))

x=c(1:7)

pdfGum = function(x,a,b){
  
  fx = (1/b)*exp(-( (x-a/b)+ exp(-(x-a/b) ) ) )
  return(fx)
}

curve(pdfGum(x,0.5,2), ylab="f(x)",xlim=c(-5,15),ylim=c(0,0.22),col="blue",
      lwd=2,main="Gumbell Distribution PDF")

curve(pdfGum(x,1,3),from=-5,to = 15,lwd=2,col="black",add=T)
curve(pdfGum(x,1.5,3),from= -5,to = 15,lwd=2,col="red",add=T)
curve(pdfGum(x,3,4),from= -5,to = 15,lwd=2,col="orange",add=T)

legend(3.8,.20, legend= c( "a= 0.5,b = 2","a=1,b = 3", "a = 1.5,b = 3", "a = 3,b = 4"),
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

legend(2,.4, legend= c( "a = 0.5,b = 2","a = 1,b = 3", "a = 1.5,b = 3", "a = 3,b = 4"),
      col =c( "blue","black","red","orange"),lty=1,cex=0.7)



