########################################################
# Shifting, Scaling, Standardization, Normalization    #
#  and Transformation                                  #  
# Md Neaz Ali                                          #  
# Stat : 2017-18                                       #  
# Roll: 1722019                                        #
# Department of Statistics, IU                         #  
########################################################



# 100000 Random Number from Poision Distribution with lambda = 50
set.seed(1000)
x = rpois(100000,50)
x
y = dpois(x,50)
plot(x,y,pch=16,las = 1,main="X VS f(x)")

# Centering
xij.centre = x-mean(x)
# Auto Scaling
Xij.auto.scale = (x-mean(x))/sd(x) 
# Range Scaling
xij.range.scale = (x-mean(x))/(max(x)-min(x))
# Normalization
xij.normalization = (x-min(x))/(max(x)-min(x))



# Pareto Scaling
xij.pareto.scale = (x-mean(x))/sqrt(sd(x))
# Vast scaling
xij.vast.scale = ((x - mean(x))/(sd(x)))*(mean(x)/sd(x))
# Level Scaling
xij.level.scale = (x-mean(x))/mean(x)



# Log Transformation
xij.log.transform = log10(x)
# Power Transformation
xij.power.transform = x^(1/length(x))
# Box Cox transformation with lambda = 5
xij.box_cox.transform = (x^5 - 1) / 5


# Box Cox Transformation with lambda = -5 to 5
lambda = seq(-5,5)
box_cox = rep(0,100000*10)
box_cox = matrix(box_cox,nrow = 10)
for(i in 1:10){
    for(j in 1:100000){
      box_cox[i,j] = (x[j]^(lambda[i])-1)/lambda[i]
    }
}

df = data.frame(box_cox)
write.table(df,"box_cox.csv",sep=",")
