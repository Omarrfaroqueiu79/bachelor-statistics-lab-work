
Data = read.csv("D:\\Data.csv")
head(Data)

Treatment_mean = aggregate(Y~T,Data,mean)
Treatment_mean

Rank_Treatment_mean = data.frame(Mean_Rank=sort(Treatment_mean$Y))
Rank_Treatment_mean

Standard_Error_mean = ((sqrt(0.45))/(sqrt(5)))
Standard_Error_mean

#############################################################################

Critical_value = data.frame(
                                P = c(2:5),
                                SSR = c(3,3.15,3.23,3.3),
                                LSR = c(c(3,3.15,3.23,3.3)*Standard_Error_mean)
                            )
Critical_value

############################################################################

Significant = data.frame(Significant = subset(
  
  Treatment_mean_Comparision$Difference_Mean,
  Treatment_mean_Comparision$Difference_Mean>c(Critical_value$LSR)),
  
  Effect= rep(c("Significant"),length(Significant))
)

Significant

##############################################################################

Treatment_mean_Comparison= data.frame(
  
            Comparison = c("AB","AC","AD","AE","BC","BD","BE","CD","CE","DE"),

Difference_Mean = c(
  
                round(abs((Treatment_mean$Y[1]-Treatment_mean$Y[2])),2),
                round(abs((Treatment_mean$Y[1]-Treatment_mean$Y[3])),2),
                round(abs((Treatment_mean$Y[1]-Treatment_mean$Y[4])),2),
                round(abs((Treatment_mean$Y[1]-Treatment_mean$Y[5])),2),
                round(abs((Treatment_mean$Y[2]-Treatment_mean$Y[3])),2),
                round(abs((Treatment_mean$Y[2]-Treatment_mean$Y[4])),2),
                round(abs((Treatment_mean$Y[2]-Treatment_mean$Y[5])),2),
                round(abs((Treatment_mean$Y[3]-Treatment_mean$Y[4])),2),
                round(abs((Treatment_mean$Y[3]-Treatment_mean$Y[5])),2),
                round(abs((Treatment_mean$Y[4]-Treatment_mean$Y[5])),2)),

                Treatment_mean_Differ = c(Significant$Significant,"--","--"),
                Treatment_mean_Effect= c(Significant$Effect,"--","--"))

Treatment_mean_Comparison




















