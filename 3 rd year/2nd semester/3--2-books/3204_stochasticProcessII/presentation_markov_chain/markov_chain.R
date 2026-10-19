
soft_drink <- read.csv("soft_drink.csv",header = T)

Curr_state<-soft_drink$Current.State
Next_state <- soft_drink$Next.State

Curr_state <- as.factor(Curr_state)
Next_state <- as.factor(Next_state)
tpm <- table(Curr_state,Next_state)
table(Curr_state)


TPM <- round(tpm/rowSums(tpm),2)
# for case 1
# What P(x10=3)?
cprobs <- round(table(Curr_state)/sum(table(Curr_state)),2)
temp10 <- TPM
for(i in range(0,10)){
  temp10 <- temp10%*%TPM
}
q10<-cprobs%*%temp10
q10[2]
# n = 10 , will choose RC

temp_ans = TPM
for(i in range(0,15)){
  temp_ans = temp_ans%*%TPM
}
q10 =cprobs%*%temp_ans
q10[3]
# for case two
# P(x10=3|x0=1)
TPM <- round(tpm/rowSums(tpm),2)
TPM
# ------------------- for 10 days ---------------------------------------
temp_ans2 = TPM
for(i in range(0,10)){
  temp_ans2 = temp_ans2%*%TPM
}
temp_ans2[1,3]
temp_ans2 = TPM
# ---- for 9 days -------------------------------------------------------
for(i in range(0,9)){
  temp_ans2 = temp_ans2%*%TPM
}
# Next_state
# Curr_state  Coca Cola Pepsi RC Sprite
# Coca Cola        14    12  3     16
# Pepsi             1     0  0      0
# RC                1     0  1      0
# Sprite            7     2  2      1

# -------------------- charts ----------------------------
cstate <- table(Curr_state)
cstate<- cstate/sum(cstate)
piepercent = cstate*100
pie(cstate, labels = piepercent,radius=2, main = "Initial Distribution of Soft Drinks",cex.main=3,
    col = c("gray5","dimgray","dodgerblue","darkblue"))
legend("topright", c("Coca Cola","Pepsi","RC","Sprite"), cex = 1,
       fill = col = c("#7bed9f","#70a1ff","#dfe4ea","#3742fa"))

states = 10**6
piepercent = floor(q10*100)
pie(q10, labels = piepercent,radius=1, main = "Initial Distribution of Soft Drinks",cex.main=3,col = c(#7bed9f,#70a1ff,#ffffff,#3742fa))
legend("topright", c("Coca Cola","Pepsi","RC","Sprite"), cex = 1,
       fill = c("green","aliceblue","blue","darkcyan"))

