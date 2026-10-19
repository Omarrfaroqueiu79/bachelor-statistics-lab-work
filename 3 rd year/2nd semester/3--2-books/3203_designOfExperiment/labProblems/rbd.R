rbd = read.csv(file.choose())


# grand total G
G = sum(rbd)
# Correction Term C.T
a = nrow(rbd)
b= ncol(rbd)
C.T = G^2/(a*b)
# SST = sum sum y^2ij
SST = sum(rbd^2)-C.T
# SSBlock = sum B^2j-C.T
rsums = rowSums(rbd)
csums = colSums(rbd)
SSblock = sum(rsums^2/b)-C.T
# SScol = sum T^2i-C.T
SStreat = sum(csums^2/a)-C.T
# SSE = SST-SSblock-SStreat
SSE = SST-SSblock-SStreat

# Anova Table
# s.v   df    ss    ms      f
# Treat 5-1=4 20.39 5.0975  3.25
# Block 4-1=3 16.67 5.556
#
#
#

# 5% level of significance F4,12
F4.12 = qf(.95,4,12)
F3.12 = qf(.95,3,12)
