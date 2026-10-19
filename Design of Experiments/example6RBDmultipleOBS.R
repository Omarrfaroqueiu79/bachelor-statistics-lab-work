#____________________________________________(i)____________________________________


setwd("C:\\Users\\Omor Faruk\\Desktop\\3203-practice")      #working directory
library(readr)           #call library
df<-read.csv("example6RBD.csv")    #open data file
df
head(df)    #see first 6 rows
names(df)    #checking variable names
df$block=as.factor(df$block)      #making as factor
df$variety=as.factor(df$variety)      #making as factor
str(df)    #checking structure
r=4         # No. of block
k=3          # No. of treatment
n=3*(r*k)

v_t=aggregate(yield~variety,df,sum)   #variety total
v_t
b_t<-aggregate(yield~block,df,sum)    #block total
b_t  

G_t<-sum(df$yield)      #grand total
G_t
G_m<-G_t/(3*(r*k))            #grand mean
G_m

Total_SS<-(sum(df$yield^2))-((G_t^2)/n)
Total_SS

BlockxVariety_yield<-aggregate(yield~block+variety,df,sum)
BlockxVariety_yield

Total_SS_of_BlockxVariety<-sum(BlockxVariety_yield$yield^2)/k-(G_t^2)/(n)
Total_SS_of_BlockxVariety

Block_SS<-sum(b_t$yield^2)/(3*k)-(G_t^2)/n
Block_SS

Variety_SS<-sum(v_t$yield^2)/(3*r)-(G_t^2)/n
Variety_SS

BlockxVariety_interaction_SS<-Total_SS_of_BlockxVariety- Block_SS- Variety_SS
BlockxVariety_interaction_SS

Error_SS<-Total_SS - Total_SS_of_BlockxVariety
Error_SS

Block_MS<-Block_SS/(r-1)
Block_MS

Variety_MS<-Variety_SS/(k-1)
Variety_MS
Interaction_MS<-(BlockxVariety_interaction_SS)/((r-1)*(k-1))
Interaction_MS

Error_df=(n-1)-((r-1)+(k-1)+((r-1)*(k-1)))
Error_df

Error_MS<-Error_SS/Error_df
Error_MS

F_cal_variety<-Variety_MS/Error_MS
F_cal_variety

F_cal_interaction<-Interaction_MS/Error_MS
F_cal_interaction

F_tab_variety<-qf(0.05,k-1,Error_df,lower.tail = FALSE)
F_tab_variety

F_tab_interaction<-qf(0.05,(r-1)*(k-1),Error_df,lower.tail = FALSE)
F_tab_interaction


ANOVA_TABLE<-data.frame(SV=c("Blocks","Varities","Interaction","Error"),
                        DF=c(r-1,k-1,(r-1)*(k-1),Error_df),
                        SS=c(Block_SS,Variety_SS,BlockxVariety_interaction_SS,Error_SS),
                        MS=c(Block_MS,Variety_MS,Interaction_MS,Error_MS),
                        F_cal=c('--', F_cal_variety,F_cal_interaction, '--'),
                        F_tab=c('--', F_tab_variety,F_tab_interaction, '--')
  
)
ANOVA_TABLE


###__________________________________(ii)_____________________________________________
#___________Standard error of difference between two varietal means______________
std_error=sqrt(Error_MS)*sqrt(2/(r*3))
std_error

treatment_mean=aggregate(yield~variety,df,mean)
treatment_mean
y_bar_v1=treatment_mean$yield[1]
y_bar_v1
y_bar_v2=treatment_mean$yield[3]
y_bar_v2

t_statistic=(y_bar_v1-y_bar_v2)/std_error
t_statistic

t_tab=qt(0.05/2,Error_df,lower.tail = FALSE)
t_tab


