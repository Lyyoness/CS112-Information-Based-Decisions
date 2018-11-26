# install.packages("SparseM")
# install.packages("MatrixModels")
# install.packages("quantreg")
library(SparseM)
library(quantreg)
?rq

library(MASS)
library(Matching)
data('lalonde')
attach(lalonde)

#-----------------------
# Matching
#-----------------------

# performing genetic matching to receive balanced data sets
# from the lalonde data

X = cbind(age, educ, black, hisp, married, nodegr, re74, re75)
Y = re78
Tr = treat

genout <- GenMatch(Tr=Tr, X=X, estimand="ATT", M=1,
                   pop.size=100, max.generations=5, wait.generations=2, replace = TRUE)

# estimating our causal effect of interest using the genout weights
mout <- Match(Y=Y, Tr=Tr, X=X, estimand="ATT", Weight.matrix=genout, M=1,
              replace = TRUE)
summary(mout)

#checking if things are well balanced
mb <- MatchBalance(treat ~ age + educ + black + hisp + married +
                   nodegr + re74 + re75,
                   match.out=mout, nboots=500)
# Before Matching Minimum p.value: 0.0020368 
# After Matching Minimum p.value: 0.27499 
# Looks good!

matched_data_treat <- lalonde[mout$index.treated,]
matched_data_control <- lalonde[mout$index.control,]
#our rq formula
quantiles = c(0.05, 0.1, 0.5, 0.9, 0.95)
rq_treat <- rq(treat ~ age + educ + black + hisp + married +
                  nodegr + re74 + re75, tau=quantiles,
                  data = matched_data_treat, 
                  method="br", model = TRUE) 
rq_control <- rq(treat ~ age + educ + black + hisp + married +
                 nodegr + re74 + re75, tau=quantiles,
                 data = matched_data_control, 
                 method="br", model = TRUE) 
rq_treat$coefficients - rq_control$coefficients

