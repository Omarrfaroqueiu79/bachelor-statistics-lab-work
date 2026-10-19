#Q1(a)____Create a vector taking sequence 1 to 50 of interval 1____

v1 <- c(1:50)
v1
#_______________Q1(b)_______________create matrix

matrixv1 <- matrix(v1,10,5)
matrixv1

#____________1(c)______________
#Slice row
row1 <- matrixv1[1,]
row2 <- matrixv1[2,]
row3 <- matrixv1[3,]
row4 <- matrixv1[4,]
row5 <- matrixv1[5,]
row6 <- matrixv1[6,]
row7 <- matrixv1[7,]
row8 <- matrixv1[8,]
row9 <- matrixv1[9,]
row10 <- matrixv1[10,]

#Slice column
column1 <- matrixv1[,1]
column2 <- matrixv1[,2]
column3 <- matrixv1[,3]
column4 <- matrixv1[,4]
column5 <- matrixv1[,5]

#_______________1(d)_____Slice first 2 column and all rows
f2crow <- matrixv1[1:2,]
f2crow

#_______1(e)_____
f3to4column <- matrixv1[,3:4]
f3to4column

#___________1(f)_____Data Frame
newDataFrame <- data.frame(f2crow,f3to4column)
newDataFrame

#___________1(g)____________Importing

exporting <- write.csv(newDataFrame, "F:\\data.csv", row.names =F)

#___________1(h)_______
newData <- read.csv(choose.files())
View(newData)
first11obs <- head(newData,11)
first11obs



#______2(a)_____Students Data

studentsName <- c("Omar","Saurav","Titas")
studentRoll <- c(21,31,09)
studentSession <- c("18-19","18-19","18-19")
studentData <- data.frame(studentsName,studentRoll,studentSession)
studentData


#______2(a)_____Students Data

studentsName <- c("Omar","Saurav","Titas","Poritosh")
studentRoll <- c(21,31,09,10)
studentSession <- c("18-19","18-19","18-19","18-19")
studentData01 <- data.frame(studentsName,studentRoll,studentSession)
studentData01
#______2(b)_____Students Data

studentsName <- c("Omar","Saurav","Titas","Lotif")
studentRoll <- c(21,31,09,17)
studentSession <- c("18-19","18-19","18-19","18-19")
studentData02 <- data.frame(studentsName,studentRoll,studentSession)
studentData02

#________2(c)__________Combination of V & H

verticalData <- cbind(studentData01,studentData02)
verticalData

horizontalData <- rbind(studentData01,studentData02)
horizontalData

#_____2(d)______GPA & YEAR

gpa <- c(3.80,3.75,3.70,3.40)
year <- c("3rd","3rd","3rd","3rd")
addgpayear <- cbind(studentData01,gpa,year)
addgpayear



#________3(a)____________Iris Data

irisData <- iris
irisData

#____________3(b)___scatter Plot
plot(irisData)






#___________wrong_______________#
#####################################

#______2(b)_____Students Data

studentsName <- c("Omar","Saurav","Kakoly","Lotif")
studentRoll <- c(21,31,42,17)
studentSession <- c("18-19","18-19","18-19","18-19")
studentData02 <- data.frame(studentsName,studentRoll,studentSession)
studentData02


#---------2(a)---------------#
studentsName <- c("Omar","Saurav","Poritosh")
studentRoll <- c(21,31,10)
studentSession <- c("18-19","18-19","18-19")
studentData <- data.frame(studentsName,studentRoll,studentSession)
studentData




#________2(c)__________Combination of V & H

verticalData1 <- cbind(studentData02,studentData)
verticalData1

horizontalData <- rbind(studentData02,studentData)
horizontalData


#_____2(d)______GPA & YEAR

gpa <- c(3.80,3.75,3.70)
year <- c("3rd","3rd","3rd")
addgpayear <- cbind(studentData01,gpa,year)
addgpayear



#________3(a)____________Iris Data

irisData <- iris
irisData

#____________3(b)___scatter Plot

