install.packages('ISLR')
library(ISLR)
set.seed(1)

#selecting indexes for a subset
train <- sample(392,196)
attach(Auto)

#checking the MSE for a linear model
lm.fit <- lm(mpg ~ horsepower, data=Auto, subset=train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

#checking MSE for a polynomial and cubic regression
lm.fit2 <- lm(mpg ~ poly(horsepower ,2),data=Auto , subset=train)
mean((mpg -predict (lm.fit2 ,Auto ))[- train]^2)

lm.fit3=lm(mpg ~poly(horsepower ,3),data=Auto , subset=train)
mean((mpg -predict (lm.fit3 ,Auto ))[- train]^2)

#---------------
#CROSSVALIDATION
#---------------
#if we use the glm model instead, we can use cv.glm()
#to perform cross validation. The family='binomial' allows
#us to do logistic regression with the model instead.
glm.fit <- glm(mpg ~ horsepower, data=Auto)
coef(glm.fit)

#we import cv.glm from the boot library
install.packages('boot')
library(boot)

cv.error <- cv.glm(Auto, glm.fit)
#delta has the CV results
cv.error$delta

#let's test the cv results for models of different fits
#results are stored in the cv.errors vector
cv.errors <- rep(0,5)

for (i in 1:5){
  glm.fit <- glm(mpg ~ poly(horsepower ,i), data=Auto)
  cv.errors[i] <- cv.glm(Auto ,glm.fit)$delta [1]
}
cv.errors

#we can also use this to do k-fold CV (standard is LOOCV)
set.seed(17)
cv.error.5 <- rep(0,5)
for (i in 1:5){
  glm.fit <- glm(mpg ~poly(horsepower ,i),data=Auto)
  cv.error.5[i] <- cv.glm(Auto ,glm.fit ,K=10) $delta [1]
}
cv.error.5

#----------
#BOOTSTRAP
#----------
#this is taken from the example of the portfolios earlier
#first we create a function that estimates the parameters
#that we are interested in, then we use boot() on it
alpha.fn <- function (data ,index){
  X <- data$X[index]
  Y <- data$Y[index]
  return ((var(Y)-cov(X,Y))/(var(X)+var(Y) -2*cov(X,Y)))
}

#for example, this line estimates alpha based on the first
#100 examples of the portfolio dataset
alpha.fn(Portfolio ,1:100)

#this is the same as constructing a new bootstrap data set
set.seed(1)
alpha.fn(Portfolio ,sample (100,100, replace=T))

#we could run the above many times, or just use boot()
boot(Portfolio ,alpha.fn,R=1000)

#this does the same for the horsepower coefficients above
boot.fn <-function (data ,index)
  return(coef(lm(mpg ~horsepower ,data=data , subset=index)))

boot.fn(Auto ,1:392)
boot(Auto ,boot.fn ,1000)

#and for the quadratic model
boot.fn <- function (data ,index)
  coefficients(lm(mpg ~horsepower +I(horsepower ^2),data=data ,
                    subset=index))

boot(Auto ,boot.fn ,1000)

#preclass work
set.seed(12)
sample_means <- rep(10000,0)
for (i in 1:10000){
  set.seed(i)
  sample_data <- rnorm(15, 0, 1)
  boot_data <- boot(sample_data, norm.fn, 10)
  sample_means[i] <- boot_data$t0
}
mean(sample_means)

norm.fn <- function(data, index)
  mean(data, subset=index)

t.test <- function(data, index)
  mean(dt(data, length(data)-1))
boot_object <- boot(sample_data, t.test, 1000)
boot_object$t0

