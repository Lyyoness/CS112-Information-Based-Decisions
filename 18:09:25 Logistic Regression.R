#loading data
library(Matching)
data('lalonde')
lalonde$treat <- factor(lalonde$treat, labels = ("noTreatment", "Treatment"))

#looking at data
names(lalonde)
dim(lalonde)
summary(lalonde)
cor(lalonde[,1:11])
plot(lalonde$re78, lalonde$nodegr)

#logistic regression
glm_model <- glm(treat~age+educ+black+hisp+married+nodegr+re78+u74+u75, data=lalonde, family=binomial) #generalized linear model (familiy=binear means that it is logistic)
summary(glm_model) 
#looking at p values, anything but re78 and nodegr is not significant

#propensity scores
glm_probs <- predict(glm_model, type="response")
glm_probs
contrasts(lalonde$treat)
glm_pred <- rep('Treatment', 445)
glm_pred
glm_pred[glm_probs < .5] <- "NoTreatment"
glm_pred

#creating a confusion matrix
table(glm_pred, lalonde$treat) #must have both with the same labels so it can plot them against each other
