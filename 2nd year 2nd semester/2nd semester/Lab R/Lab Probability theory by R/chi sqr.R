# Chi-square distribution
set.seed(1822021)
x <- rchisq(500, 5)
#hist(x, prob=TRUE,xlim = c(0,40))
plot( density(x), col='orange',xlim = c(0,35))
curve( dchisq(x, df=10), col='green', add=TRUE)
curve( dchisq(x, df=15), col='red', add=TRUE )

legend(17,0.15, legend = c("df1=5", "df2=10", "df3=15"),col=c("green", "blue","red"), lty=1, cex = 1)