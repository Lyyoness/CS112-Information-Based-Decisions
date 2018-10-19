install.packages('foreign')
library(foreign)
turnout <- read.dta("Google_Drive/School/Minerva/Classes/2018:2019/CS112/4.1 Preclass/turnout.dta")

lm2 <- glm(turnout ~ age + agesqrd + educate + income + white, data = turnout, family='binomial')
lm2$coefficients
nonlog <- sum(lm2$coefficients * c(1, 38, 14.44, 16, 4, 0))

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
logit2prob(nonlog)

nonlog_means <-sum(lm2$coefficients * c(1, 38, 14.44, 16, mean(turnout$income), mean(turnout$white)))
logit2prob(nonlog_means)

sim_probs <- sim(lm2, n.sim=1000)
 
storage[i] <- exp(sum(simulated.coefs[i,]*Xs))/(1 + exp(sum(simulated.coefs[i,]*Xs))); 
quantile(sim_probs, probs = c(0.005, 0.995))
