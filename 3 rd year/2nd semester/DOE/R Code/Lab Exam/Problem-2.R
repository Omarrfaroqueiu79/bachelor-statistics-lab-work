setwd("C:\\Users\\Omor Faruk\\Desktop\\Lab Exam")
library(readxl)
df=read_excel("Problem-2.xlsx")
df

head(df)

df$Treatment=as.factor(df$Treatment)
str(df)

r=4
n=r*r

Grand_total=sum(df$Yield)
Grand_total

G=Grand_total^2/n
G

Total_ss=sum(df$Yield^2)-G
Total_ss

Row_t=aggregate(Yield~Rows,df,sum)
Row_t

Row_ss=sum(Row_t$Yield)/r-G
R