setwd("C:\\Users\\Omor Faruk\\Desktop\\Lab Exam")
library(readxl)
df=read_excel("Problem-01.xlsx")
df

head(df)

df$Treatment=as.factor(df$Treatment)
str(df)

#______________________________Analysis of RBD____________________

r=6
k=4
n=2*r*k
n

Grand_total=sum(df$Yield)
Grand_total

G=Grand_total^2/n
G

Block_total=aggregate(Yield~Block,df,sum)
Block_total

Treatment_total=aggregate(Yield~Treatment,df,sum)
Treatment_total

Total_ss=sum(df$Yield^2)-G
Total_ss

BlockxTreatment=aggregate(Yield~Block+Treatment,df,sum)
BlockxTreatment

Total_ss_of_BlockxTreatment=sum(BlockxTreatment$Yield^2)/2-G
Total_ss_of_BlockxTreatment

Block_ss=sum(Block_total$Yield^2)/(2*k)-G
Block_ss

Treatment_ss=sum(Treatment_total$Yield^2)/(2*r)-G
Treatment_ss

BlockxTreatment_interaction_SS=Total_ss_of_BlockxTreatment- Block_ss- Treatment_ss
BlockxTreatment_interaction_SS

Error_ss=Total_ss- Total_ss_of_BlockxTreatment
Error_ss


Block_ms=Block_ss/(r-1)
Block_ms

Treatment_ms=Treatment_ss/(k-1)
Treatment_ms

Interaction_ms=BlockxTreatment_interaction_SS/((r-1)*(k-1))
Interaction_ms

Error_df=(n-1)-((r-1)+(k-1))-((r-1)*(k-1))
Error_df

Error_ms=Error_ss/Error_df
Error_ms


F_cal_Block=Block_ms/Error_ms
F_cal_Block

F_cal_interaction=Interaction_ms/Error_ms
F_cal_interaction

F_tab_Block=qf(.05,r-1,Error_df,lower.tail = F)
F_tab_Block

F_tab_Interaction=qf(.05,((r-1)*(k-1)),Error_df,lower.tail = F)
F_tab_Interaction


#__________________Mean comparison______________
#_______________H0:mu(C)=mu(D)________________
#_______________H1:mu(C)!=mu(D)________________

Treatment_mean=aggregate(Yield~Treatment,df,mean)
Treatment_mean

s=sqrt(Error_ms)
s
se=s*sqrt(2/r)
se

y_bar_C=Treatment_mean$Yield[3]
y_bar_D=Treatment_mean$Yield[4]

Diff=y_bar_C- y_bar_D
Diff

t=Diff/se
t
t_tab=qt(.05/2,Error_df,lower.tail = F)
t_tab
