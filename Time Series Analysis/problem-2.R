data("Airpassenger")
AirPassengers

#____Understanding and Preparing data___
boxplot(AirPassengers~cycle(AirPassengers))
plot(AirPassengers)
abline(lm(AirPassengers~time(AirPassengers)))

#___Make it stationary___

plot(log(AirPassengers),col="green",lwd=3,ylab="Passengers") #variance constant

#_____First order differentiation for making mean zero____

plot(diff(log(AirPassengers)),col="yellow",lwd=3,ylab="Passengers")
#_______mean zero and variance constant___our data now Stationary

#___________After making stationary we can fit ARIMA model__________
#__Modeling: ARIMA(p,q,d)
# ARIMA(p=Order of auto regressive,d=order of differentiation,
#q= order of moving average)

#_____we need tseries library_____
library(tseries)
#________Selecting the value of q(MA)_________
acf(AirPassengers) #autocorrelation plot of original ts data file
acf(diff(log(AirPassengers))) ##autocorrelation plot of stationary ts data

#Here the number of line cross or touch the blue line is the value of q
#we ignore the first line for zero value and the next after 1 line because
#data is seasonal.i.e. q=1 

#__________Selecting the value of p______
pacf(diff(log(AirPassengers)))

#_____Fitting an ARIMA(0,1,1) model___
# help(arima)

my_fit=arima(log(AirPassengers),order = c(0,1,1),seasonal =list(order=c(0,1,1),period=12))
my_fit

#Forecast for next 10 years

pred=predict(my_fit,n.ahead = 10*12)
pred
#____final prediction___
#__our fitted model data was log transform data so we get log value from forecast.
#__Then,convert to original by exponentiation

finalpred=exp(pred$pred)
ts.plot(AirPassengers,finalpred,log="y",lty=c(1:3),col=c(3,'red'))

#_________Checking accuracy of the prediction_________
train_data=ts(AirPassengers,frequency = 12,start=c(1949,1),end = c(1959,12))

my_fit2=arima(log(train_data),order = c(0,1,1),seasonal =list(order=c(0,1,1),period=12))
my_fit2

#_____Predict for one year____
pred2=predict(my_fit2,n.ahead = 1*12)
pred2
pred3=exp(pred2$pred)
pred3

pred_1960=round(pred3,0)
true_1960=tail(AirPassengers,12)
df=data.frame(pred_1960,true_1960)
df

# The difference between the true value and the predicted value is minor or negligible
# hence the forecast is well.
