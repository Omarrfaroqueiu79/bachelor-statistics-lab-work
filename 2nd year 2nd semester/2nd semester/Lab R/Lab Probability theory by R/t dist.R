# t distribution
set.seed(1822021)
x <- rt(500, 15)
#hist(x, prob=TRUE,ylim = c(0,0.45))
plot( density(x), col="Orange",ylim = c(0,0.4))
curve(dt(x, 20), col = "Green", add=TRUE)
curve(dt(x, 35), col = "Red", add = TRUE)

legend(.9,0.39, legend = c("df1=15", "df2=20", "df3=35"),col=c("Orange", "Green","Red"), lty=1, cex = .8)