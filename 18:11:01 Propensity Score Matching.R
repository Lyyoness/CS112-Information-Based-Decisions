library(foreign)

RCT_data <- read.dta("http://www.nber.org/~rdehejia/data/nsw.dta")

RCT_diffmeans <- 	mean(RCT_data$re78[RCT_data$treat == 1]) - 
  mean(RCT_data$re78[RCT_data$treat == 0])
print(RCT_diffmeans)

summary(lm(RCT_data$re78 ~ RCT_data$treat)) # note the coef on “treat”
confint(lm(RCT_data$re78 ~ RCT_data$treat)) # note the confint on “treat”

fake_obs <- read.dta("dw_data.dta")
attach(fake_obs)
dim(fake_obs)

head(fake_obs,5)
library(Matching)

glm1  <- glm(treat ~ age + I(age^2) + education + I(education^2) + black +
               hispanic + married + nodegree + re74  + I(re74^2) + re75 + I(re75^2), family=binomial, data=fake_obs)

prop_scores <- glm1$fitted.values

matches <- Match(Tr=treat, X=prop_scores, ties=FALSE)
summary(matches)
matches$index.treated
matches$index.control

# basic Mahalanobis distance:
X = cbind(age + I(age^2) + education + I(education^2) + black +
  hispanic + married + nodegree + re74  + I(re74^2) + re75 + I(re75^2))
distancematching = Match(Tr = treat, X = X, ties =  FALSE)
distancematching$index.control




