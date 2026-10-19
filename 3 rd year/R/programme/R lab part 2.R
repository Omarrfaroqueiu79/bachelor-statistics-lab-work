# read data in R
iris
#2data view
View(iris)
# few data from iris data 
f<-head(iris,10)
print(f)
plot(f)
#method of correlation matrix
cor(iris)
cor(iris,method="pearson")
cor(iris,method="spearman")

#stepwise regression:
#using swiss data
#need a package:
library(MASS)
#call default data 
swiss
View(swiss)
#linear regression model where fertility is dependent variavable 

model1<-lm(swiss$Fertility~.,data = swiss)
print(model1)
summary(model1)

##-----------stepwise regression-------------------

#forward,backward and both
forward<-step(model1,direction ="forward")
summary(forward)
backward<-step(model1,direction = "backward")
summary(backward)
both<-step(model1,direction = "both")
summary(both)

#pridiction of the model
names(model1)
model1$coefficients

#------multicollinearity checking -----------
vif(model1)















