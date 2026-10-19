#------------Stochastic lab

#-----------Problem -1:----------
#the given transition matrix----
p<-c(.7,0.3,.4,.6)
p1<-matrix(p,nrow = 2,byrow = T)
print(p1)
#### Now we have find out the p1*p1
product1<-p1%*%p1
print(product1)

##Hence we have to find also p2*p2.that rain in fourth days 
product2<-product1%*%product1
print(product2)

# the probability P00^4 is 
product2[1,1]


#-------------problem -2 consider a markov chain ----------------
##---
v<-c(3/4,1/4,0,1/4,2/4,1/4,0,3/4,1/4)
print(v)
p<-matrix(v,nrow = 3,byrow = T)
print(p)
#####-------now product of the matrix ------------
p.p<-p%*%p
print(p.p)
########### the   value of P01,p02-------------------
p01<-p.p[1,2]
p01
p02<-p.p[1,3]
print(p02)

#####----------------identity matrix -----------
diag(5)
#----------problem 8-----------------




