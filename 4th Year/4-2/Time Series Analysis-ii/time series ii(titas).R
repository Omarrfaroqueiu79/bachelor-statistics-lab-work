library(tidyverse)
library(urca)
library(stargazer)
library(vars)
library(haven)
library(tsDyn)
library(tsutils)
denmark
data_var=denmark[,c("LRM","LRY","IBO","IDE")]
view(data_var)
var_matrix=cbind(denmark$LRM,denmark$LRY,denmark$IBO,denmark$IDE)
var_matrix
colnames(var_matrix,do.NULL = FALSE)

colnames(var_matrix)=c("LRM","LRY","IBO","IDE")

#plots
par(col="skyblue",col.axis="mediumpurple",mfrow=c(2,2),mar=c(2,2,2,2))
ts.plot(data_var$LRM)
ts.plot(data_var$LRY)
ts.plot(data_var$IBO)
ts.plot(data_var$IDE)

#lag lenght selection
VARselect(var_matrix,lag.max = 5,type = "const",season = 4)

#VAR estimation  var(1)
var1=VAR(var_matrix, p = 1, type = "const", season = 4)
summary(var1)

#Var goodness of fit
#stability check
stability_var1=stability(var1)
par(col="skyblue",col.axis="mediumpurple",mfrow=c(2,2))
plot(stability_var1$stability$LRM)
plot(stability_var1$stability$LRY)
plot(stability_var1$stability$IBO)
plot(stability_var1$stability$IDE)

sctest(var1)

#autocorrelation check
serial.test(var1,lags.pt = 8)
#so autocorrelation foud in var1
