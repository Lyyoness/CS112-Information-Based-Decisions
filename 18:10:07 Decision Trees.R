install.packages("tree")
library(tree)

#we create a binary variable for if sales>8
#this is the label we try to predict
library(ISLR)
attach(Carseats)
High <- ifelse(Sales<=8, "No", "Yes")
Carseats <- data.frame(Carseats, High)

#tree() has essentially the same syntax as lm()
# - removes the specific term -> -1 removes intercept
# . is "all columns not otherwise in the formula"
tree.cs <- tree(High ~ .-Sales, Carseats)
summary(tree.cs)

#we can plot this!
plot(tree.cs)
text(tree.cs, pretty = 0)

#we can also just print the tree object's details
tree.cs

#but we should do CV for proper results
#we use subset with indexes in the actual training
set.seed(2)
train <- sample(1: nrow(Carseats), 200)
Carseats.test <- Carseats[-train ,]
High.test <- High[-train]

tree.crossval <- tree(High ~ .-Sales, Carseats, subset=train)
tree.pred <- predict(tree.crossval, Carseats.test, type='class')
#and follow with confusion matrix
table(tree.pred, High.test)
(86+57)/200

#let's see if pruning would help
#FUN = prune.misclass says we want to use classification error
#to guide pruning instead of deviance
set.seed(3)
cv.carseats <- cv.tree(tree.crossval, FUN = prune.misclass)
names(cv.carseats)
#size is number of nodes in tree, dev error rate (even though
#it's called deviance here b/c we used the FUN argument)
#k is Alpha
cv.carseats

#let's plot error rate as a function of size and k:
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type='b')
plot(cv.carseats$k, cv.carseats$dev, type='b')

#let's prune the tree to that
prune.carseats <- prune.misclass(tree.cs, best=9)
plot(prune.carseats)
text(prune.carseats ,pretty =0)

#testing how this does on test data
tree.pred.pruned <- predict(prune.carseats, Carseats.test, type='class')
table(tree.pred.pruned, High.test)
(101+74)/200


#------------------------
# On Lalonde Data
#------------------------
library(Matching)
data('lalonde')
train <- sample(1:nrow(lalonde), nrow(lalonde)/2)
unempl.tree <- tree(u75 ~ .-re78, lalonde, subset=train)
summary(unempl.tree)

plot(unempl.tree)
text(unempl.tree, pretty=0)


#------------------------
# On Pina Data
#------------------------
library(MASS)
library(tree)
pima.train <- Pima.tr
pima.test <- Pima.te
pima.tree <- tree(type ~ ., pima.train)
summary(pima.tree)

cv.prima <- cv.tree(pima.tree, FUN = prune.misclass)
cv.prima
par(mfrow=c(1,2))
plot(cv.prima$size, cv.prima$dev, type='b')
plot(cv.prima$k, cv.prima$dev, type='b')

prune.prima <- prune.misclass(pima.tree, best=5)
summary(prune.prima)
plot(prune.prima)
text(prune.prima, pretty=0)

tree.pred <- predict(pima.tree, pima.test, type='class')
tree.pred.pruned <- predict(prune.prima, pima.test, type='class')
table(tree.pred, pima.test$type)
table(tree.pred.pruned, pima.test$type)
(175+65)/332
(193+58)/332
