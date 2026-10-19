#________________Analyzing and forecasting monthly rainfall of Bangladesh______
df=read.csv(file.choose(),header = T,sep = ",")
df
head(df)
View(df)
#Packages
# install.packages("lubridate")
library(ggplot2)
library(dplyr)
library(lubridate)
library(forecast)

#________Preparing data for analysis_________
df1<-filter(df,df$Year>="2007")
df1=filter(df1,Station=="Rajshahi")
View(df1)

df2=select(df1,Rainfall)
df2

#_________Convert the data from integer to continuous format____
df2$Rainfall=as.numeric(df2$Rainfall)

##_________Convert the data into a time series data____
RD=ts(df2$Rainfall,start = 2007,frequency = 12)
RD

#________Time series Plot monthly rainfall in Rajshahi_____

plot(RD)

# here mean and variance more or less constant,
# seasonal component exists

#____________Decompose_________
# For visualize underline components of time series dazta
RDR=decompose(RD)
plot(RDR)

#______Fitting an ARIMA model____
fit_model=auto.arima(RD)
fit_model

#________Forecast________
pred=forecast(fit_model,4*12)

#_____plot observed and forecast value together___
autoplot(pred,PI=F)

# Here, black colored lines are observed time series plot
# and blue colored lines are forecasted time series plot 


# Showing the forecasted value with 80% and 95% CI
autoplot(pred,PI=T)

#_______remove negative value by visualize plot from 0_____
autoplot(pred,PI=T)+
  ylim(0,550)

