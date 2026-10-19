
df<-iris
df
veiw(df)
names(df)
#------------fist 10 data---------------#
f<-head(df,10)

f
par(mfrow=c(2,2))
#-----scatter plot----------------
plot(f$Petal.Width,f$Sepal.Width,
     main = "Petal.width and Spepal.Width",
     xlab = "Petal.Width",
     ylab = "Speal.width",
     col=4,
     pch=14
     )
plot(f$Petal.Width,f$Sepal.Length)

#----------------correlation matrix---------------#

# -----Regrssion----------------
model1=lm(f$Petal.Width~f$Sepal.Length+f$Sepal.Width+f$Petal.Length)
model1
#--------backward resssion ---------------
model2=lm(f$Petal.Width~f$Petal.Length+f$Sepal.Width+f$Sepal.Length)
model2


 ### residual analysis-------------------#
res1=residuals(model1)

res1
par(mfrow=c(1,1))
plot(res1)
##-------plot residual-------------#

par(mfrow=c(2,2))
plot(model1$residuals,model1$fitted.values)
plot(model2$residuals,model2$fitted.values)








