#1. read dataset in R
data = iris
data
#2.view the iris dataset in R

View(data)
#.3 first few observations with variables in R 
head(data,12)

# 4. Scatter Diagram 

x = head(data$Petal.Length,200)
y = head(data$Petal.Width,200)

plot(x,y,col = 1,pch = 19, cex = 1,main = "Scatter Diagram of iris dataset",
     xlab = "Petal.width",ylab = "Petal.lengh")

# 5. correlation matrix
install.packages("corrplot")
library(corrplot)

b=cor(data[-5]) # remove no of 5 column
b
corrplot(b,method = "number")
cor(b)
#6. Multicolinearity checking in R

x1 = iris$Sepal.Length #Sepal.Length will be treated as independent variable
x2 = iris$Sepal.Width #Sepal.Width will be treated as independent variable
x3 = iris$Petal.Length #Petal.Length will be treated as independent variable
y = iris$Petal.Width #Petal.Width will be treated as dependent variable

MLREG=lm(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, iris)
summary(MLREG)

vif(MLREG) # multicolinearity
# Stepwise Regression ( forward and backword)


# 9. checking all assumptions of regression

plot(MLREG)
# 10. finally pediction










