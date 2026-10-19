# Those the quaterly and monthly data are the stationary 
# cause data form normal distribution ,Have equal mean and
# variance.

#But in real life problem all data sets are not stationary
#In this situation we have to convert data into a stationary
#data


#-Detection of tend and get a stationary time series 
# install.packages("fpp")
library(fpp)
data(ausbeer)
View(ausbeer)
ausbeer

library(readxl)
df<- read_excel("GDP of bangladesh.xlsx")
View(df)
# gdp.ts=window(df,start = 1980,end = 2020)
gdp=ts(df)
plot((gdp))