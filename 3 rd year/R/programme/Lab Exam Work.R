###___(1)_____

#_____a_____
A<-c(1:50)
A
#____b_____#
mat1<-matrix(A,nrow=10,ncol=5,byrow = F)
mat1

#_____c_______#

row1<-mat1[1,]
row1
row2<-mat1[2,]
row2
row3=mat1[3,]
row3
row4=mat1[4,]
row4
row5=mat1[5,]
row5
row6=mat1[6,]
row6
row7=mat1[7,]
row7
row8=mat1[8,]
row8
row9=mat1[9,]
row9
row10=mat1[10,]
row10

########col#####
col1=mat1[,1]
col1
col2=mat1[,2]
col2
col3=mat1[,3]
col3
col4=mat1[,4]
col4
col5=mat1[,5]
col5


#_____d_____

f2colrows<-mat1[1:2,]
f2colrows


#________e_____
s3to4col<-mat1[,3:4]
s3to4col


#______f______
dataframe<-data.frame(f2colrows,s3to4col)
dataframe



####_____g_______####
##exporting<-write.csv(newDataFrame,"F\\DATA.CSV",row.names = F)



###______h_____
newData<-read.csv(choose.files())
View(newData)
first1obs<-head(newData)
first1obs










#___(2)___

##____a____
studentsinfo<-data.frame(
 StudentsNo<-c(1:3),
 Roll<-c(1822021,1822026,1822020),
 Name<-c("Omar","Parbej","Asad"),
 Session<-c(201819,201819,201819))
studentsinfo


##______b____
studentsinfo2<-data.frame(
  StudentsNo<-c(1:4),
  Roll<-c(1822017,1822009,1822025,1722026),
  Name<-c("Lotif","Titas","Hafeez","Munna"),
  Session<-c(201819,201819,201819,201718))
studentsinfo2



