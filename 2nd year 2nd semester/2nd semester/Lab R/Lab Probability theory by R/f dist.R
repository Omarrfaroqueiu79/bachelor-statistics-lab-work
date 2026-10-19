# F distribution
set.seed(1822021)
x <- rf(500, 5, 7)
#hist(x, prob=TRUE)
plot( density(x), col='orange',xlim = c(0,8) ,ylim = c(0,0.75))
curve(df(x, 5, 17), col = "green", add=TRUE)
curve(df(x, 25, 7), col = "red", add = TRUE)

legend(3.5,0.74, legend = c("df1=5", "df2=10", "df3=15"),col=c("green", "blue","red"), lty=1, cex = 1)