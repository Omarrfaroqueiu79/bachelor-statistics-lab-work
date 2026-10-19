#chi square
set.seed(21)
x<-rchisq(500, 5)
plot(density(x), col="green", xlim=c(0,40))
curve(dchisq(x, 10), col="blue", add=T)
curve(dchisq(x, 15), col="red", add=T)

legend(15,0.14, legend = c("df1=5", "df2=10", "df3=15"),col=c("green", "blue","red"), lty=1, cex = 1)