#1.read in r
iris
#2.View 
View(iris)
#3.first few observation:
head(iris)
#4.scatter plot
plot(iris)
#5.correlation matrix:
iris$Species<-as.numeric(iris$Species)
iris
cor(iris)
#method of correlation
cor(iris,method = "pearson")
cor(iris,method = "kendal")
cor(iris,method = "spearman")
#correlation matrix:
iris$Species<-as.numeric(iris$Species)
install.packages("psych")
library(psych)
corr.test(iris)
corr.test(iris,method = "pearson")
corr.test(iris,method = "kendal")
corr.test(iris,method = "spearman")
#stepwise regression:
#using swiss data:
#need a package:
installed.packages("MASS")
library(MASS)
#call default data 
swiss
View(swiss)
#make a linear model where fertility is dependent variable:
model1<-lm(swiss$Fertility~.,data=swiss)
model1
summary(model1)
#forword:
step.forward<-stepAIC(model1,direction = "forward",trace = 1)
summary(step.forward)
step.forward<-stepAIC(model1,direction = "backward",trace = 1)
summary(step.forward)
step.forward<-stepAIC(model1,direction = "both",trace = 1)
summary(step.forward)
step.forward<-stepAIC(model1,direction = c("forward","backward","both"),trace = 1)
summary(step.forward)


#########or
forward<-step(model1,direction = "forward")
summary(forward)
backward<-step(model1,direction = "backward")
summary(backward)
both<-step(model1,direction = "both")
summary(both)


#prediction
predict(model1)
#residual
resid(model1)
#or:
residuals(model1)
resid(model1)==residuals(model1)

