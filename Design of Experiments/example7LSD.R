#____________________________________________(i)____________________________________

setwd("C:\\Users\\Omor Faruk\\Desktop\\3203-practice")      #working directory
library(readr)           #call library
df<-read.csv("example7LSD.csv")    #open data file
df
head(df)    #see first 6 rows
names(df)    #checking variable names
df$rows=as.factor(df$rows)
df$columns=as.factor(df$columns)
df$treatment=as.factor(df$treatment)      #making as factor
str(df)    #checking structure
r=5

grand_total=sum(df$yield)
grand_total

row_total=aggregate(df$yield~df$rows,df,sum)
row_total

column_total=aggregate(df$yield~df$columns,df,sum)
column_total

treatment_total=aggregate(yield~treatment,df,sum)
treatment_total

#------------sum of squares--------------------
total_ss=sum(df$yield^2)-(grand_total^2)/(r*r)
total_ss
row_ss=(sum(row_total$`df$yield`^2)/r)-(grand_total^2)/(r*r)
row_ss
column_ss=sum(column_total$`df$yield`^2)/r-(grand_total^2)/(r*r)
column_ss
treatment_ss=sum(treatment_total$yield^2)/r-(grand_total^2)/(r*r)
treatment_ss
error_ss=total_ss-row_ss-column_ss-treatment_ss
error_ss

########--------mean sum squares-------------------######
row_ms=row_ss/(r-1)
row_ms
col_ms=column_ss/(r-1)
col_ms
treat_ms=treatment_ss/(r-1)
treat_ms
error_df=((r*r)-1)-((r-1)+(r-1)+(r-1))
error_ms=error_ss/error_df
error_ms

#---------------F_calculated values--------------

F_cal_treat=treat_ms/error_ms
F_cal_treat

#-----------F-critical value-------------
F_tab_treat=qf(0.05,r-1,error_df,lower.tail = FALSE)
F_tab_treat

#--------------p value-------------
p_value=pf(65.47,r-1,error_df,lower.tail = FALSE)

#-----------------anova table-------------
anova_table=data.frame(s_v=c("Rows","Columns","Treatments","Error"),
                 DF=c(r-1,r-1,r-1,error_df),
                 SS=c(row_ss,column_ss,treatment_ss,error_ss),
                 MS=c(row_ms,col_ms,treat_ms,error_ms),
                 F_cal=c("  ","  ",F_cal_treat,"  "),
                 F_tab=c("  ","  ",F_tab_treat,"  "),
                 p_valu=c("  ","  ",p_value,"  ")
                 )
anova_table



###___________________________________(ii)__________________________________________

##################-----mean comparison of A and D treatment-------###########

#----null hypothesis:Ho:mu(A)=mu(D)
#---alternative hypothesis :is not equal

treatment_mean=aggregate(yield~treatment,df,mean)
treatment_mean 

y_bar_A=treatment_mean$yield[1]
y_bar_D=treatment_mean$yield[4]

diff=(y_bar_A-y_bar_D)
diff
s=sqrt(error_ms)
s
std_error=s*sqrt(2/r)
std_error
T_statistic=diff/std_error
T_statistic
#--------------t_tabulated value--------------------
T_tab=qt(0.05/2,error_df,lower.tail = FALSE)
T_tab
round(T_tab,2)


#_______________________________________________(iii)__________________________________________

##______________________Efficiency_of_LSD_relative_to_RBD_with_rows_as_blocks_is_given_by__________

RE_r=(column_ss+((r-1)+error_df)*error_ms)/(((r-1)+(r-1)+error_df)*error_ms)
RE_r

if(RE_r>1){
  print("LSD has been effective in reducing error variation")
} else if(RE_r<1)
    {
    print("LSD has not been effective in reducing error variation")
  
}





##______________________Efficiency_of_LSD_relative_to_RBD_with_columns_as_blocks_is_given_by__________


RE_c=(row_ss+((r-1)+error_df)*error_ms)/(((r-1)+(r-1)+error_df)*error_ms)
RE_c


if(RE_c>1){
  print("LSD has been effective in reducing error variation")
} else if(RE_c<1)
{
  print("LSD has not been effective in reducing error variation")
  
}




##______________________Efficiency_of_LSD_relative_to_CRD__________________

RE=(row_ss+column_ss+((r-1)+error_df)*error_ms)/((r^2-1)*error_ms)
RE

if(RE>1){
  print("LSD has been effective slightly in reducing error variation over CRD")
} else if(RE<1)
{
  print("LSD has not been effective slightly in reducing error variation over CRD")
  
}

