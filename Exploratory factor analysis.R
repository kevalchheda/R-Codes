#install.packages("GPArotation")
library(psych)
library(GPArotation)

# Data ability.cov available in R. This data is
ability.cov # six tests (general - vocabulary ) were given to 112 

efa_1 <- ability.cov$cov #covariance matrix of the variable

efa_cor <- cov2cor(efa_1) #convert the covariance matrix to correlation

efa_cor

# To find the number of factors required for analysis

fa.parallel(efa_cor, n.obs = 112, fa = "both", n.iter = 100)

#To find the principal axis unrotated

fa <- fa(efa_cor, nfactors= 2, rotate = "none", fm = "pa")

fa

# To find the principal axis : orthogonal rotation (factors unrelated)

fa.varimax <- fa(efa_cor, nfactors = 2, rotate = "varimax", fm = "pa")

fa.varimax

fa.diagram(fa.varimax) # To plot the factor diagram

# To find the principal axis : oblique rotation (factors related)

fa.promax <- fa(efa_cor, nfactors=2, rotate="promax", fm="pa")

fa.promax

fa.diagram(fa.promax) # To plot the factor diagram
