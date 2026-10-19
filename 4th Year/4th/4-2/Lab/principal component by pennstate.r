df<-read.csv(choose.files(),header = T)
df
cor_matrix<-cor(df)
cor_matrix
head(df)
write.table(cor_matrix,file = "cor_matrix",sep = " ")
install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(cor_matrix)
ggcorrplot
installed.packages("princomp")
library(princomp)
data_pca<-princomp(cor_matrix,cor = T)
data_pca
summary(data_pca)
eigen_value<-eigen(cor_matrix)
eigen_value
eigen_value$values
eigenvectors<-data_pca$loadings
eigenvectors
plot(data_pca,type = "b", col="red")
biplot(data_pca)
install.packages("factoextra")
library(factoextra)
fviz_eig(data_pca, addlabels = TRUE, ylim = c(0, 50))
fviz_contrib(data_pca,choice = "var",axes = 1,top = 5)
fviz_contrib(data_pca,choice = "var",axes = 2,top = 5)
fviz_contrib(data_pca,choice = "var",axes = 3,top = 10)
fviz_contrib(data_pca,choice = "var",axes = 1:2,top = 10)
seree_plot<-screeplot(data_pca,type = "l")

