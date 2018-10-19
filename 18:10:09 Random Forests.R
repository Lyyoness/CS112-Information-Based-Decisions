library(tree)
library(ISLR)
attach(Carseats)

#basic classification tree from last time
High=ifelse(Sales <=8,"No","Yes ")
Carseats =data.frame(Carseats ,High)
tree.carseats =tree(High~.-Sales , Carseats )
summary(tree.carseats )
plot(tree.carseats )
text(tree.carseats ,pretty =0)

#Regression tree from last time
library(MASS)
set.seed(1)
train = sample (1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston , subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston , pretty =0)

#-------------------------
#Random Forest and Bagging
#-------------------------
install.packages("randomForest")
library(randomForest)
set.seed(1)

#bagging (RF with m=p)
#mtry = m here (p=13)
#importance makes it assess variable importance
bag.boston = randomForest(medv~ ., data=Boston, subset=train,
                          mtry=13, importance=TRUE)
bag.boston

#how does this perform on test set?
ypred.bag = predict (bag.boston , newdata=Boston[-train ,])
boston.test=Boston[-train ,"medv"]
plot(ypred.bag , boston.test)
abline (0,1)
#MSE
mean((ypred.bag -boston.test)^2)

#Random Forest works exactly the same, except that we use
#mtry = m = sqrt(p)
set.seed(1)
rf.boston= randomForest(medv~.,data=Boston , subset=train ,
                          mtry=6, importance =TRUE)
ypred.rf = predict(rf.boston ,newdata=Boston[-train ,])
mean((ypred.rf-boston.test)^2)

#we can now get the importance scores
importance(rf.boston)
#IncMSE is measures by mean decrease in accuracy,
#Purity measures total decrease of node impurity

#We can plot this
varImpPlot(rf.boston)

#------------
#Boosting
#------------
install.packages('gbm')
library (gbm)
set.seed(1)

#distribution here means we are doing regression, if we did
#binary classification, we'd use "bernoulli"
#depth limits how deep the trees can grow
boost.boston=gbm(medv~.,data=Boston[train ,], distribution=
                     "gaussian",n.trees=5000, interaction.depth=4)
#this automatically plots the importance
summary(boost.boston)

#variable dependence plot
par(mfrow=c(1,2))
plot(boost.boston ,i="rm")
plot(boost.boston ,i="lstat")

#
ypred.boost=predict (boost.boston ,newdata =Boston[-train ,],
                    n.trees=5000)
mean((ypred.boost - boston.test)^2)

