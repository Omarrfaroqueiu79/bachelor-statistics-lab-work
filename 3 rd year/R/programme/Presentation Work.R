######Cricket score analysis######
text="The Data Of Some Opener Cricketer"
cat(text)

##Average Score Analysis

Name<-c("Babar Azam","Rizwan","Liton Das","Tamim Iqbal","Conway","Rohit Sharma","Warner","Finch","Malan","Hanry Brook","Joss Buttler")
Age<-c(27,30,28,33,31,34,35,35,35,23,32)

##HEIGHT IN FEET,INCHES
Height<-c(5.11,5.8,5.7,5.9,5.11,5.6,5.6,5.7,5.9,5.9,5.11)

##WEIGHT IN kg
Weight<-c(67,65,65,64,66,72,65,90,72,73,73)
AverageRun<-c(43.2,53.2,22.2,24.1,57.0,31.5,33.9,34.5,41.0,35.1,33.0)

x<-data.frame(Name,Age,Height,Weight,AverageRun)
print(x)
plot(x$Height)
plot(x)
multiple.regression<-lm(AverageRun~Age+Height+Weight,data = x)
summary(multiple.regression)
