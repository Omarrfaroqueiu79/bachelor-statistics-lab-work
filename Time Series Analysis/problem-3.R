#__________Decomposition of time series analysis______
#____Using Air Passengers Data______

data("AirPassengers")
View(AirPassengers)
AirPassengers

#___________Preparing time series data__________
AP=ts(AirPassengers,frequency = 12,start = c(1949,1))

#________Understanding data_______
# __________Components of a time series_________
# Trend(T)
# Seasonal(S)
# Cyclical(C)
# Irregular(I) or Random 
plot(AP)

#____Fix-up variation using log-transform______
AP2=log(AP)
plot(AP2) #here we see the variance are same

#__________Decompose__________
DAP=decompose((AP2))
DAP$figure
plot(DAP$figure,
     type='b',
     xlab = 'Month',
     ylab = 'Seasonality Index',
     col="blue",
     las=2)
#_________Decompose plot_______ 
plot(DAP)

#___________ARIMA(p,d,q) model_____________
library(forecast)
fit_model=auto.arima(AP2)
fit_model

#________Residual plot__________
hist(fit_model$residuals,
     main = 'Residual plot',
     xlab = 'Error',
     col='blue',
     freq=F)
lines(density(fit_model$residuals),
      col='red',lw=3)

#__________Forecast for the next 4 years____
pred=forecast(fit_model,4*12)
#library(ggplot2)
autoplot(pred) #deep blue 80% CI and light blue 95% CI

#______Checking accuracy of the predicted value_______
accuracy(pred)

