set.seed (1)
#------------------------------------------
#General parameters
#------------------------------------------
J <- 15
n <- J*(J+1)/2
group <- rep (1:J, 1:J)
mu.a <- 5
sigma.a <- 2
a <- rnorm (J, mu.a, sigma.a)
b <- -3
x <- rnorm (n, 2, 1)
sigma.y <- 6
y <- rnorm (n, a[group] + b*x, sigma.y)
u <- runif (J, 0, 3)
y123.dat <- cbind (y, x, group)
plot(x,y)

#------------------------------------------
# Linear regression
#------------------------------------------
x1 <- y123.dat[,2] #same as x
y1 <- y123.dat[,1]

M1 <- lm (y1 ~ x1)
display(M1)
M1.sim <- sim(M1)
coef.M1.sim <- coef(M1.sim) #simulating coefficients and stdev for the model
sigma.M1.sim <- sigma.hat(M1.sim)

## to get the uncertainty for the simulated estimates

apply(coef(M1.sim), 2, quantile) #uncertainty for coefficients
quantile(sigma.hat(M1.sim)) #sigma.hat gets the residual errors from a fitted model


#------------------------------------------
# Logistic regression
#------------------------------------------
u.data <- cbind (1:J, u)
dimnames(u.data)[[2]] <- c("group", "u")
u.dat <- as.data.frame (u.data)  #this is messy af?
y <- rbinom (n, 1, invlogit (a[group] + b*x)) #WHY DON'T YOU MAKE A NEW Y?
plot(x,y)
M2 <- glm (y ~ x, family=binomial(link="logit"))
display(M2)
M2.sim <- sim (M2)
coef.M2.sim <- coef(M2.sim)
sigma.M2.sim <- sigma.hat(M2.sim)
