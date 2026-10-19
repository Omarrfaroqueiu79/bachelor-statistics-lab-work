#Required packages
install.packages("psych")
library(psych)
my_data<-bfi
View(my_data)
my_data<-my_data[complete.cases(my_data)]
KMO(my_data)
mydata<-my_data[,KMO(my_data)$MSAi>.05]
cortest.bartlett(mydata)
#Factor Analysis
is.na(my_data)
my_data<-na.omit(my_data)
my_data
factanal(my_data,3,rotation = "none")
fit<-factanal(my_data,3,rotation = "varimax")
fit
fit1<-fa(mydata,
        nfactors = 7,
        rotate = "varimax",
        fm="pa",
        residuals = T)
fit1
print(fit1$loadings,cutoff=.3,sort=T)
#determining the number of factors
install.packages("nFactors")
library(nFactors)
eig<-eigen(cor(my_data))
eig
ap<-parallel(subject = nrow(my_data),var = ncol(my_data),rep = 100,cent = .05)
ns<-nScree(x=eig$values,aparellel=ap$eigen$quevpea)
plotnScree(ns)

