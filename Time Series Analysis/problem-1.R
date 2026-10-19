#______________________Time Series Analysis______________________
#________Generating 100 random numbers from normal distribution________
#__________with mean 0 and variance 1___________ 
x<-rnorm(100,0,1)
x

help(ts)

#________Creating a quarterly time series data_______
quarter_ts<-ts(data=x,start=1995,frequency = 4)
quarter_ts
#___Creating time series plot for quarterly data_____
plot(quarter_ts,col="blue",ylab="Frequency",
     xlab="Year",main="Quarterly Time series Plot")

#________Creating a monthly time series data_______
monthly_ts<-ts(data=x,start=1995,frequency = 12)

#___Creating time series plot for monthly data_____
plot(monthly_ts,col=6,ylab="Frequency",
     xlab="Year",main="Monthlyly Time series Plot")   

#______Detection of trend and get a stationary time series data______
#install.packages("fpp")
library(fpp)
library(forecast)
data(ausbeer)
View(ausbeer)
ausbeer

beer.ts=window(ausbeer,start=1956,end=1975) #_A part from data file
beer.ts
plot(as.ts(beer.ts)) # Upward trend data

#_______Creating a moving average that will be close to trend

beer.trend=ma(beer.ts,order = 4,centre = T)
#________Plot trend and MA together______
plot(as.ts(beer.ts))
lines(beer.trend)

#______Removing the trend from the time series______
beer.detrend=beer.ts-beer.trend

#___________Plot the detrended data_____
plot(as.ts(beer.detrend))


#__Now our plot is stationary plot because here over the time__
#___mean is same and variance is also same_____