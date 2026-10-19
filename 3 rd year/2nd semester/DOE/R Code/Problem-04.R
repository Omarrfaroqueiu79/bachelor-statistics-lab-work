##____________________Problem_04____________________________________

setwd("C:\\Users\\Omor Faruk\\Desktop\\Non Parametric Lab\\Excel")
d<-read.csv("Chi-square data.csv")
d

phead=sum(d$X*d$Observed)/400
phead

PX0=round((choose(4,0)*(phead)^0*(1-phead)^(4-0)),4)
PX0

PX1=round((choose(4,1)*(phead)^1*(1-phead)^(4-1)),4)
PX1

PX2=round((choose(4,2)*(phead)^2*(1-phead)^(4-2)),4)
PX2

PX3=round((choose(4,3)*(phead)^3*(1-phead)^(4-3)),4)
PX3

PX4=round((choose(4,4)*(phead)^4*(1-phead)^(4-4)),4)
PX4

Y=data.frame(PX=c(PX0,PX1,PX2,PX3,PX4))
Y
ExpectedX0=sum(d$Observed)*PX0
ExpectedX0

ExpectedX1=sum(d$Observed)*PX1
ExpectedX1

ExpectedX2=sum(d$Observed)*PX2
ExpectedX2


ExpectedX3=sum(d$Observed)*PX3
ExpectedX3

ExpectedX4=sum(d$Observed)*PX4
ExpectedX4

Z=data.frame(Expected=c(ExpectedX0,ExpectedX1,ExpectedX2,ExpectedX3,ExpectedX4))
Z

#________Final Data_____________
cbind(d,Y,Z)


Q4=sum((d$Observed-Z$Expected)^2/Z$Expected)
Q4

Chi_tab=qchisq(.05,3,lower.tail = F)
Chi_tab

if(Q4>Chi_tab){
  print("Null hypothesis is rejected")
} else if(Q4<Chi_tab)
{
  print("Fail to reject null hypothesis")
  
}
