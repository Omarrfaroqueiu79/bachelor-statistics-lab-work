# Matrix
x=c(5,2,1,4,55)
x
sort(x)

y=c(6,7,8,9,10)
y
plot(x,y)
line(density(y))


y[-3]
x[5]
rep(seq(1,33,by=2),times=3)
rep(c("latif","Omaar"),times=5)
p<-seq(-5,5,by=.3)
rep(p,times=5)
seq(length=22,from=1,by=.5)
rep(x,each=3)
B=matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3,byrow =F)


#matrix(c(1,2,3,4,5,6,7,8,9),n row = 3, n col=3, by row =T)
rownames = c("row1", "row2", "row3")
colnames = c("col1", "col2", "col3")
A=matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3,byrow =T,dimnames = list(rownames,colnames))
A
B=matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3,byrow =F,dimnames = list(rownames,colnames))
B
x=A*B
x
print(x[2,2])
print(x[,3])

# Divide the matrices
result <- A/B
cat("Result of division","\n")
print(result)



help(solve)
help.start

R=c(1,8,9,2)
R
ave=mean(R)
ave
median(R)


sort(x)