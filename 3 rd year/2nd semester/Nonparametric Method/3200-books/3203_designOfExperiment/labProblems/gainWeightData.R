
weightGainData = read.csv(file.choose())
View(weightGainData)

c1 = weightGainData$A
c2 = weightGainData$B
c3 = weightGainData$C
c4 = weightGainData$D


As = rep("A",5)
Bs = rep("B",5)
Cs = rep("C",5)
Ds = rep("D",5)

Treatments = rbind(As,Bs,Cs,Ds)
Respose = rbind(c1,c2,c3,c4)

Treatments = matrix(Treatments,nrow=1,byrow = F)

Treatments = c(As,Bs,Cs,Ds)
Response = c(c1,c2,c3,c4)
model1 = lm(Response~Treatments)
summary(model1)
anova(model1)
#------------------------------------------------------------------------------------------weightGainData = read.csv(file.choose())
weightGainData = read.csv(file.choose())
View(weightGainData)
Asq = weightGainData$A^2
Bsq = weightGainData$B^2
Csq = weightGainData$C^2
Dsq = weightGainData$D^2


gtotal <- sum(weightGainData)
cterm <- sum(Asq,Bsq,Csq,Dsq)
cterm2 = cterm/(nrow(weightGainData)*ncol(weightGainData))
summary(weightGainData)
sum(weightGainData)
