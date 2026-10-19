
##################################
###### Graph Empirical PDF #######
##################################

#############################
### Pareto Rayleigh ###
############################

##########################################
#### Airplane  ####
##########################################



x= c( 24.52, 69.15, 33.21, 30.79, 18.12, 94.34, 55.40, 28.28,
      49.73, 15.92, 21.81, 21.80, 30.56, 18.56, 87.63, 4)


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
# CdfParetodist=function(x,a,k)
# {
#   Fx=(1-(k/x)^a)
#   return(Fx)
# }
# PR <- Fx
# 
# lines(x,PR)
# 




###### Pareto
Fx=function(x,k,a)
Fx <- 1-(k/x)^a

lines(x,Fx(x,1,1),col="red",lty=4,lwd=2.5)
lines(x,Fx(x,1,2),col="blue",lty=4,lwd=2.5)
lines(x,Fx(x,1,3),col="green",lty=4,lwd=2.5)



##################
####  Legend  ####
##################

legend(150,.4,legend=c("ECDF","PR ","PE ","P "), 
       col=c("blue","red","black","green"),
       lty=c(1,1,1,4), ncol=1,cex = 0.65)

###################
#### Thank You ####
###################