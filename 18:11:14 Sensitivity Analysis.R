install.packages("rbounds")
library(MASS)
library(Matching)
library(rbounds)

library(foreign)
DW_data <- read.dta("nsw_dw.dta")
?GenMatch
#------------------------------------
# Start with GenMatching
#------------------------------------
attach(DW_data)
Y <- re78 #the outcome of interest
Tr <- treat #the treatment of interest

#Now With GenMatch
X <- cbind(age, education, black, hispanic, married, nodegree, re74, re75)

#Genetic Weights
gen1 <- GenMatch(Tr=Tr, X=X, pop.size=200, M=1, print=0, replace=TRUE)

#Match
mgen1 <- Match(Y=Y, Tr=Tr, X=X, Weight.matrix=gen1, M=1,replace=TRUE)
summary(mgen1)
mb <- MatchBalance(treat ~ age + education + black + hispanic +
                     married + nodegree + re74 + re75,
                   match.out=mgen1, nboots=500)

#---------------------------------
# Sensitivity Analaysis
#---------------------------------
psens(mgen1, Gamma=1.2, GammaInc=.05)
#Gamma = 1.15 sensitivity

hlsens(mgen1, Gamma=1.2, GammaInc=.05)
#Gamma = 1.15      Lower Bound = 587.29      Upper bound = 1518.5
# This can be roughly interpreted as the difference in medians
# across treatment and control groups