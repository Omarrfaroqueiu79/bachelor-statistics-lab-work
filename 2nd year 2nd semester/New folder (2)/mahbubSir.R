# n=500, df=5 ,df=15, df=20

x=rt(500,5)
plot(density(x),col="red",ylim=c(0,.5))
curve(dt(x,15),add=T,col="green")
curve(dt(x,20),add=T)

#n=500 df1=5,df2=7 df1=7 df2=17, df1=25 df2= 6

x=rf(500,5,7)
plot(density(x),ylim=c(0,.8))
curve(df(x,7,17),add=T,col="green")
curve(df(x,25,6),add=T)

#n=500, df=10

x=rchisq(500,5)
plot(density(x),ylim=c(0,.4),xlim=c(0,40))
curve(dchisq(x,7),add=T,col="green")
curve(dchisq(x,20),add=T)

