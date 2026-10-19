data1<-rep(LETTERS[1:5], c(7,4,2,9,11))
data1
mytable<-table(data1)
mytable
barplot(mytable)


#______1(a)_______________

mydata<-rep(LETTERS[1:5],c(7,4,2,9,11))
mydata
mytable<-table(mydata)
mytable
barplot(mytable)


mytable2<- sort(mytable,decreasing=TRUE)
mytable2

barplot(mytable2,main ="BarPlot", xlab="Letters", ylab="Quantity", col= "red",border = "green")
#____________1(C)_____________

mdata<-rep(LETTERS[1:5],c(7,4,2,9,11))
mdata
mytable<-table(mdata)
mytable
barplot(mytable,main = "Bar Plot",xlab="latters",ylab = 'numbers', col = "black")
d<-sort(mytable,decreasing = F)
d
barplot(d,)


k<-c(4,5,6,8,3)
k
h<-c(1:100)
h


