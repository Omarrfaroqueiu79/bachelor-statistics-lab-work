setwd("F:\\Documents\\Book\\1\\3 rd year\\2nd semester\\DOE\\R Code\\Practical\\New folder\\Lab\\Myself")
library(readxl)
df<-read_excel("p-6.xlsx")
df
names(df)
head(df)
df
df$Variety=as.factor(df$Variety)
str(df)

r=4
k=3
n=3*r*k

Grand_total=sum(df$Yield)
Grand_total

G=Grand_total^2/n
G

Variety_total=aggregate(Yield~Variety,df,sum)
Variety_total

Block_total=aggregate(Yield~Block,df,sum)
Block_total

Total_ss=sum(df$Yield^2)-G
Total_ss

BlockxVariety=aggregate(Yield~Block+Variety,df,sum)
BlockxVariety

Total_ss_of_BlockxVariety=sum(BlockxVariety$Yield^2)/k-G
Total_ss_of_BlockxVariety

Block_ss=sum(Block_total$Yield^2)/(3*k)-G
Block_ss

Variety_ss=sum(Variety_total$Yield^2)/(3*r)-G
Variety_ss   

BlockxVariety_interaction_ss=Total_ss_of_BlockxVariety-Block_ss-Variety_ss
BlockxVariety_interaction_ss

Error_ss=Total_ss- Total_ss_of_BlockxVariety
Error_ss

Block_Ms=Block_ss/(r-1)
Block_Ms

Variety_Ms=Variety_ss/(k-1)
Variety_Ms

Interaction_Ms=BlockxVariety_interaction_ss/((r-1)*(k-1))
Interaction_Ms

Error_df=(n-1)-((r-1)+(k-1))-((r-1)*(k-1))
Error_df
Error_Ms=Error_ss/Error_df
Error_Ms

F_cal_variety=round((Variety_Ms/Error_Ms),2)
F_cal_variety

F_cal_Interaction=round((Interaction_Ms/Error_Ms),2)
F_cal_Interaction

F_tab_Variety=round(qf(.05,(k-1),Error_df,lower.tail = F),2)
F_tab_Variety

F_tab_interaction=round(qf(.05,(r-1)*(k-1),Error_df, lower.tail = F),2)
F_tab_interaction


###################__________________________ANOVA TABLE________________________

Anova_table=data.frame(
  SV=c("Block","Variety","Interaction","Error"),
  DF=c(r-1,k-1,((r-1)*(k-1)),Error_df),
  SS=c(Block_ss,Variety_ss,BlockxVariety_interaction_ss,Error_ss),
  MS=c(Block_Ms,Variety_Ms,Interaction_Ms,Error_Ms),
  F_cal=c(' ', F_cal_variety, F_cal_Interaction, ' '),
  F_tab=c(' ', F_tab_Variety, F_tab_interaction, ' ')
)
Anova_table
