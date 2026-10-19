set.seed(22)

x= rt(500,df=20)
plot(density(x),ylim=c(0,0.6),col="blue",lwd=3,xlab="x",ylab = "f(x)",main = "T Dist ")
curve(dt(x,df=15),col="red",lwd=3,add=T)
#curve(dt(x,df=40),col="pink",lwd=3,add=T)
curve(dt(x,df=35),col="green",lwd=3,add=T)

 legend(1,0.6,legend = c("n=500,df=50,","n=500,df=30"
                         
        ,"n=500,df=40","n=500,df=35"),col=c("blue","red","pink","green"),lty=1,cex=1.5)
