n=3 #no. of row
p=2 #no.of variable/col
mu_p_0=as.matrix(c(9,5))
mu_p_0
X=matrix(c(6,9,10,6,8,3),nrow = 3,byrow = T)
X
X_bar=as.matrix(colMeans(X))
X_bar

#mean_diff
m_diff=X_bar-mu_p_0
m_diff
#transpose
m_tr=t(m_diff)
m_tr

s=cov(X)
s
#Hotelling T2
s_inv=solve(s)
s_inv
T2=n*m_tr%*%s_inv%*%m_diff
T2
#tabulated F  value
F_tab_value=qf(.10,df1 = p,df2 = n-p,lower.tail = FALSE)
F_tab_value
# Now calculate the critical value
F_critical=(((n-1)*p)/(n-p))*F_tab_value
F_critical
