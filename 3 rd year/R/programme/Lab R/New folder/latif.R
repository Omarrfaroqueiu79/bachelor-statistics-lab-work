#matrix operation
v1=c(1:9)
m1=matrix(v1,nrow = 3,byrow = T)
m1
colnames=c("c1","c2","c3")
rownames=c("r1","r2","r3")

p=matrix(m1,nrow=3,byrow = T,dimnames =list(colnames,rownames))
print(p)

print(m1)
m2=m1^-1
print(m2)









