
##################################
###### Graph Empirical PDF #######
##################################

#############################
### Pareto Rayleigh ###
############################

##########################################
#### Airplane  ####
##########################################



x= c( 23, 261, 87, 7, 120, 14, 62, 47, 225, 71, 246, 21, 42, 20, 5, 12, 120, 11,
      3, 14, 71, 11, 14, 11, 16, 90, 1, 16, 52, 95)


v= sort(x)
x=v
hist(x)




########################
### Empirical CDF ######
########################


ecdf(x)

plot.stepfun(ecdf(x),main = "",
             xlab = "x ", ylab = "Fn(x)" , col.vert = "blue",col = "blue",cex=.001,lwd=2)



###### Pareto-Rayleigh
xm <- min(x)
s <- 1.5000000 
a <- 0.1886536 
Fx <- 1-(xm^a)*(xm+(1/2)*s^(-2)*x^2)^(-a)
PR <- Fx

lines(x,PR,col="red",lty=1,lwd=2.5)


###### Pateto-Exponential
xm <- min(x)
l <- 1.5000000
k <- 1
a <- 0.3267589   
Fx <- 1-(xm^a)*(xm+l^(-k)*x^k)^(-a)
PE <- Fx



lines(x,PE,col="black",lty=1,lwd=2)

###### Pareto
xm <- min(x)
a <- 0.2977888 
Fx <- 1-(xm/x)^a
P <- Fx
lines(x,P,col="green",lty=4,lwd=2.5)


##################
####  Legend  ####
##################

legend(150,.4,legend=c("ECDF","PR ","PE ","P "), 
       col=c("blue","red","black","green"),
       lty=c(1,1,1,4), ncol=1,cex = 0.65)

###################
#### Thank You ####
###################