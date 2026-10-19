knitr::opts_chunk$set(echo = TRUE)
install.packages("tidyverse")
install.packages("urca")
install.packages("stargazer")
install.packages("vars")
install.packages("haven")
install.packages("tsDyn")
install.packages("tsutils")


library(tidyverse) 
library(urca) 
library(stargazer) 
library(vars) 
library(haven) 
library(tsDyn) 
library(tsutils) 
data(denmark) 
data_var <- denmark[,c("LRM","LRY","IBO","IDE")] 
view(data_var) 
var_matrix <- cbind(denmark$LRM, denmark$LRY, denmark$IBO, denmark$IDE) 
colnames(var_matrix, do.NULL = FALSE)

colnames(var_matrix) <- c("LRM","LRY","IBO","IDE")

par(col = "skyblue", col.axis = "mediumpurple", mfrow = c(2,2), mar = c(2,2,2,2)) 
ts.plot(data_var$LRM) 
ts.plot(data_var$LRY) 
ts.plot(data_var$IBO) 
ts.plot(data_var$IDE)

VARselect(var_matrix, lag.max = 5, type = "const", season = 4)


var1 <- VAR(var_matrix, p = 1, type = "const", season = 4) 
summary(var1)


stargazer(var1$varresult, type = "text", style = "aer", title = "VAR(1)", 
          column.labels = c("LRM","LRY","IBO","IDE"))


stability_var1 <- stability(var1) 
par(col = "skyblue", col.axis = "mediumpurple", mfrow = c(2,2)) 
plot(stability_var1$stability$LRM) 
plot(stability_var1$stability$LRY) 
plot(stability_var1$stability$IBO) 
plot(stability_var1$stability$IDE)


sctest(var1)

serial.test(var1, lags.pt = 8)



var2 <- VAR(var_matrix, p = 2, type = "const", season = 4) 
summary(var2)

stargazer(var2$varresult, type = "text", style = "aer", title = "VAR(2)", 
          column.labels = c("LRM","LRY","IBO","IDE"))


stability_var2 <- stability(var2) 
par(col = "skyblue", col.axis = "mediumpurple", mfrow = c(2,2)) 
plot(stability_var2$stability$LRM) 
plot(stability_var2$stability$LRY) 
plot(stability_var2$stability$IBO) 
plot(stability_var2$stability$IDE)


sctest(var2)

serial.test(var2, lags.pt = 8)


irf_var2 <- irf(var2, n.ahead = 16, ortho = FALSE,impulse = 
                  c("LRM","LRY","IBO","IDE"), response = c("LRM","LRY")) 
plot(irf_var2)


#irf_var2$irf$LRM 
fevd_var2 <- fevd(var2, n.ahead = 8) 
par(col = "mediumpurple", col.axis = "mediumpurple", mfrow = c(4,1), mar = 
      c(2,2,2,2)) 
plot(fevd_var2)

fevd_var2$LRM

for_var2 <- predict(var2, n.ahead = 8) 
par(col = "skyblue", col.axis = "mediumpurple", mfrow = c(4,1), mar = c(2,2,2,2)) 
plot(for_var2)


fanchart(for_var2,  
         colors = c("skyblue4","skyblue3","skyblue2","skyblue1","skyblue", 
                    "purple4","purple3","purple2","purple1"),  
         col.y = "skyblue")



time <- ts(denmark$ENTRY, deltat = 1/4, start = c(1974,1), end = c(1987, 3)) 
season_dummy <- seasdummy(nrow(data_var), y = time) 
colnames(season_dummy) <- c("Q1", "Q2", "Q3") 
view(season_dummy)

diff_var <- diff(as.matrix(data_var), lag = 1)

var1_diff <- lineVar(var_matrix, lag = 1, include = "const", model = "VAR", I = 
                       "diff", exogen = season_dummy) 
summary(var1_diff)

for_season <- seasdummy(nrow(data_var)+12, y = time) 
for_hor_seas <- for_season[56:67,1:3] 
var1_diff_fcst <- predict(var1_diff, n.ahead = 12, exoPred = for_hor_seas) 
forecasts <- rbind(var_matrix, var1_diff_fcst) 
par(mfrow = c(4,1), mar = c(2,2,2,2)) 
names <- c("LRM","LRY","IBO","IDE")  
for(i in 1:4)  
{ 
  matplot(forecasts[,i], type=c("l"), col = c("skyblue"),  
          main = names[i] ) 
  abline(v= nrow(var_matrix), col="mediumpurple") 
}

VARselect(diff_var, lag.max = 5, type = "const", season = 4)