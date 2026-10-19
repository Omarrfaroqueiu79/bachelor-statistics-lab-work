
# Daw a sample size n=500, from f distribution with 
# degrees of freedom r n1=5 n2=7 and plot. Hence over another tools
# sample of same size with degrees
# of freedoms are n1=5 n2=17 and n1=25 n2=6 respectively and comment on your result


set.seed(1)

par(mfrow = c(0,3))

x <- rf(500, df1 = 25, df2 = 7)
hist(x, 
     breaks = 'Scott', 
     freq = FALSE, 
     xlim = c(0,3), 
     ylim = c(0,1),
     xlab = '', 
     main = 'F distribution n1=25 n2=7 ')

curve(df(x, df1 = 25, df2 = 7), from = 0, to = 4, n = 500, col= 'blue', lwd=2, add = T)


set.seed(1)

x <- rf(500, df1 = 5, df2 = 17)
hist(x, 
     breaks = 'Scott', 
     freq = FALSE, 
     xlim = c(0,3), 
     ylim = c(0,1),
     xlab = '', 
     main = 'F distribution n1=5 n2=17 ')

curve(df(x, df1 = 5, df2 = 17), from = 0, to = 4, n = 500, col= 'blue', lwd=2, add = T)


set.seed(1)

x <- rf(500, df1 = 5, df2 = 7)
hist(x, 
     breaks = 'Scott', 
     freq = FALSE, 
     xlim = c(0,3), 
     ylim = c(0,1),
     xlab = '', 
     main = 'F distribution n1=5 n2=7 ')

curve(df(x, df1 = 5, df2 = 7), from = 0, to = 4, n = 500, col= 'blue', lwd=2, add = T)