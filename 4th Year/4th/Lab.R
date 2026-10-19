setwd("C:/Users/Omor Faruk/Desktop/4-2")
# Problem_1
# Read the data 
df=read.csv("Amitriptyline_Data.csv",header = T, sep = ",")
View(df)
# y1 is the first dependent variable
y1=df$Y1
# Extract second column from the data frame y2 as second Dependent variable
y2=df$Y2
# Extract independent variable
x=df[c(3:7)]
print(x)

# (a) Perform a regression analysis using only the first response y1
# 1
#fitting the linear regression models
model1=lm(y1~df$Z1+df$Z2+df$Z3+df$Z4+df$Z5)

print(model1)
summary(model1)
# Prediction interval
new_data=c(1,1200,140,70,85)
pred=predict(model1,data=new_data)
pred

#2
# Using the second response
#fitting the linear regression models
model2=lm(y2~df$Z1+df$Z2+df$Z3+df$Z4+df$Z5)

print(model2)
summary(model1)
# Prediction interval
new_data=c(1,1200,140,70,85)
pred=predict(model2,data=new_data)
pred

#3
#Multivariate multiple regression 
mul_reg=lm(cbind(Y1,Y2)~Z1+Z2+Z3+Z4+Z5,data = df)

# Univariate analysis
summary(mul_reg)
coef(mul_reg)

# Multivariate test
install.packages("car")
library(car)
summary(Anova(mul_reg))

# Residuals /Normality
# Mardia's test Analyzes the multivariate skewness and
# kurtosis of a distribution. This test is based on the statistics 
# for skew and kurtosis,which are derived from the 
# matrix \(m2=R=(X-X)\prime S-1(X-X)\)

head(residuals(mul_reg))
library(MVN)
mvn(residuals(mul_reg),univariateTest="SW",mvnTest="mardia")

hist(residuals(mul_reg))
boxplot(residuals(mul_reg))

# Problem 2
# Principle component analysis
library("ggplot2")
library("ggfortify")
library("carData")
library("factoextra")
df1=read.csv("places_tf.csv",header = T,sep=",")
head(df1)
summary(df1)
# 
df_pca=prcomp(df1,scale=TRUE)
print(df_pca)
summary(df_pca)
# Finding the eigenvalues
e_val=get_eigenvalue(df_pca)
e_val

head(e_val)
# Scree Plot
fviz_eig(df_pca,col.var="read")
# PCA results for variables
var=get_pca_var(df_pca)
var

# Factor analysis from df1
# For FA we need a package psych 
#install.packages("psych")
library(psych)
fa=fa(df1,nfactors = 10,rotate = ("varimax"))
summary(fa)
# view the factor loadings
fa$loadings
# display variance explained by each factor
print(fa$Vaccounted)
# residuals
fa$residual
#
fa$communalities
#

