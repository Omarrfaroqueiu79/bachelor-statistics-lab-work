
df <- read.csv(file.choose(),header = T)
head(df)
dim(df)
View(df)

names(df)
rownames(df) <- df$ID_REF

head(df)
View(df)

df2 = df[,-1]
View(df2)
dim(df2)


heatmap(df2)
df3 <- as.numeric(df2)
heatmap(df3)
str(df2)
cData = df2

heatmap(df2[1:100,])
res = t.test(df2[1,1:17],df2[1,18:34])
res$p.value
dim(df2)
pvals = rep(0,22277)

for(i in 1:22277){
  pv= t.test(cData[i,1:17],cData[i,18:34])
  pvals[i] = pv$p.value
}

pvals[1]
length(pvals)
par(mfrow=c(17,2))
for(i in 1:34){
  plot(1:22277,df2[,i])
}



class(cData)



#------------------ New Analysis 27/5/23 ------------------------

df[1:4,]
View(df)
dim(df)
rownames(df) <- df[,1]
head(df)


cData = df[,-1]
View(cData)


heatmap(as.matrix(cData))
