# # TTTTTT
# set.seed(1017)
# x=rt(500,15)
# plot(density(x),ylim=c(0,.4),col="blue")
# curve(dt(x,20),add=T)
# curve(dt(x,35),add=T)
# 
# 
# # CHI
# x=rchisq(1000,7)
# plot(density(x),ylim=c(0,.4),col="blue")
# curve(dchisq(x,11),add=T)
# curve(dchisq(x,17),add=T)
# 
# #
x=rnorm(1000,10,5)
plot(density(x),ylim=c(0,.8))
curve(dnorm(x,10,5/sqrt(100)),add=T)








