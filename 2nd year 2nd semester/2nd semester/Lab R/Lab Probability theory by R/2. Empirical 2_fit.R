
##################################
###### Graph Empirical PDF #######
##################################

#############################
### Pareto ###
############################
par(mfrow=c(1,2))

##########################################
#### Airplane  ####
##########################################



x= c( 23, 261, 87, 7, 120, 14, 62, 47, 225, 71, 246, 21, 42, 20, 5, 12, 120, 11,
      3, 14, 71, 11, 14, 11, 16, 90, 1, 16, 52, 95)

n=30
v= sort(x)
x=v
#hist(x)




########################
### Empirical CDF ######
########################


ecdf(x)

plot.stepfun(ecdf(x),main = "",
             xlab = "x ", ylab = "Fn(x)" , col.vert = "blue",col = "blue",cex=.001,lwd=2)



###### Pareto distribution

k <- 1 
a <- 1
FX<-(1-(k/x)^a)
P<-FX
lines(x,P,col="red",lty=1,lwd=2.5)

###### Pareto distribution

k <- 1 
a <- 2
FX<-(1-(k/x)^a)
P<-FX
lines(x,P,col="green",lty=1,lwd=2.5)

###### Pareto distribution

k <- 1 
a <- 3
FX<-(1-(k/x)^a)
P <- FX
lines(x,P,col="yellow",lty=1,lwd=2.5)


##################
####  Legend  ###
##################

legend(150,.4,legend=c("ECDF","P ","P ","P "), 
       col=c("blue","red","green","yellow"),
       lty=c(1,1,1,4), ncol=1,cex = 0.65)

###################
#### Thank You ####
###################


####Empirical PDF####
plot(x, (1:n)/n, type = 's', ylim = c(0, 1),xlab = "", ylab="Fn(x)", main = NULL)


###### Pareto distribution

k <- 1 
a <- 1

fx<-((a*(k)^a)/x^(a+1))
P<-fx
lines(x,P,col="red",lty=1,lwd=2.5)

###### Pareto distribution

k <- 1 
a <- 2
fx<-((a*(k)^a)/x^(a+1))
P<-fx
lines(x,P,col="green",lty=1,lwd=2.5)

###### Pareto distribution

k <- 1 
a <- 3
fx<-((a*(k)^a)/x^(a+1))
P<-fx
lines(x,P,col="yellow",lty=1,lwd=2.5)


##################
####  Legend  ###
##################

legend(150,.4,legend=c("EPDF","P ","P ","P "), 
       col=c("blue","red","green","yellow"),
       lty=c(1,1,1,4), ncol=1,cex = 0.65)
