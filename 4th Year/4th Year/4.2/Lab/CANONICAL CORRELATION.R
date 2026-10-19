install.packages("ggplot2")
install.packages("GGally")
install.packages("CCA")
install.packages("psych")
installed.packages("CCP")
library(readxl)
library(ggplot2)
library(GGally)
library(CCA)
library(CCP)
df<-read.csv(choose.files(),header = T)
summary(df)
psych<-df[,1:3]
acad<-df[,4:8]
ggpairs(psych)
ggpairs(acad)
#correlations
matcor(psych,acad)
#R-canonical correlation analysis
cc1<-cc(psych,acad)
#Display the ccninical correlation analysis
cc1$cor
##row canonical coefficint
cc1[3:4]
#compute canonical loadings
cc2<-comput(psych,acad,cc1)
#display canonical loadings
cc2[3:2]
#install.packages(CCP)
#tests for canonical correlaion dimensions
rho<-cc1$cor
##define number of observations, number of variables in first set and second set
n<-dim(psych)[1]
p<-length(psych)
q<-length(acad)
##calcualtion p- valuues using the F-approximation of different tests statistics
library(dplyr)
shapiro.test(df$len)
is.numeric(df)
df1<-lapply(df,as.numeric)
shapiro.test(df$len)
str(df1)
is.numeric(df1)
p.asym(rho,n,p,q,tstat="Wilks")
p.asym(rho,n,p,q,tstat="Hotelling")
p.asym(rho,n,p,q,tstat="Pillai")
p.asym(rho,n,p,q,tstat="Roy")
##standardized psych canonical coefficient diagonal matrix of pysch of psych sd's
s1<-diag(sqrt(diag(cov(psych))))
s1%*%cc1$xcoef
##standardized acad canonical coefficient diagonal matrix of pysch of psych sd's
s2<-diag(sqrt(diag(cov(acad))))
s2%*%cc1$ycoef

