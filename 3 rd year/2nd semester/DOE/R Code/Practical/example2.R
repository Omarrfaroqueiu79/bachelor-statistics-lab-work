library(readxl)
library(agricolae)
book1 <- read_excel("CRD example/Book1.xlsx")

data = stack(book1)
data

anova = aov(values~ind,data)
anova
summary(anova)

lsd = LSD.test(anova,"ind")
lsd
