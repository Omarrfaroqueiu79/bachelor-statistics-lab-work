install.packages("readxl")
library(readxl)
blongum = read.table(file.choose(),header = T,sep = ",")
a=blongum
View(a)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
ggplot(a[,2:3], aes(x = counts)) + geom_bar(stat = "identity")
??ggplot
summary(a$cell.1)
summary(a$CELL.2)

var(a$cell.1)
install.packages("MASS")
library(MASS)
#for negative binomial model
a1=glm.nb(a$cell.1 ~ a$CELL.2)
summary(a1)
View(a1)
ggplot(a1, aes(x = counts)) + geom_bar(stat = "identity")
#for poisson model
m.poission=glm(a$cell.1 ~ a$CELL.2,family = "poisson",data = a)
summary(m.poission)
install.packages("pscl")
library(pscl)
#fit zero inflated poisson
m.zip=zeroinfl(a$cell.1 ~ a$CELL.2,data = a)
library(pscl)
summary(m.zip)

#zero inflated negative binomial
m.zinb= zeroinfl(a$cell.1~a$CELL.2,link = "logit",dist ="negbin")
summary(m.zinb)
#compare AIC of poisson and zero inlflated poisson
AIC(m.poission,m.zip)
#compare AIC of negative binomial and zinb
AIC(a1,m.zinb)
#compare four of that
AIC(m.poission,a1,m.zinb,m.zip)
??geom_histogram
