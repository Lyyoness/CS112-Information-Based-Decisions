#----------------------------------------------------------------------------------------
# Code from https://gist.github.com/anonymous/a39827bdf89a0599956f8b1d774ab3f5
# Propensity scores example from http://www.stat.columbia.edu/~cook/qr33.pdf (II-8 p.68)
# function modified to return data frame instead of list of lists
#----------------------------------------------------------------------------------------


# Function that assigns treatment/control depending on 
# propensity scores (assignment probabilities)
experiment <- function(vector.of.probabilities = NULL) {
  storage_df <- data.frame("Subject"=1:length(vector.of.probabilities),
                           'Score' = vector.of.probabilities, 'Treatment'=0 )
  k = 0
  for (i in 1:length(vector.of.probabilities)) {
    if(
      sample(x = c(1,0), size = 1, prob = c(vector.of.probabilities[i], 
                                            1 - vector.of.probabilities[i])) == 1) {
      storage.vector[k] <- i
      k = k + 1
    }
  }
  storage_df$Treatment[storage.vector] = 1
  return(storage_df)
}

# Income distribution for those with no kids
set.seed(12345); nokids.income <- round(abs(exp(rnorm(1000, 5, 1))))
hist(nokids.income, breaks=20)

# Household sizes for the female-headed households with children are defined per this code:
set.seed(123); kids.hhsize <- round(sqrt(abs(rnorm(1000, 12, 100))) + .3)
hist(kids.hhsize, breaks=20)

# Probability of assignment to treatment
probs.wnokids  <- 0.5*((((max(nokids.income) + 100) - nokids.income)/(max(nokids.income) + 100))^4)
probs.wyeskids <- kids.hhsize/(max(kids.hhsize) + 1)
hist(probs.wnokids)
hist(probs.wyeskids)

#--------------------------------------------------------------
#Preclass work:
# Use this code to simulate treatment assignment for the two
# blocks of units given in Example II-8 (page 68)
#--------------------------------------------------------------
vector_of_probs <- c(0.68, 0.42, 0.73, 0.79, 0.63,
                     0.4, 0.38, 0.44, 0.41)


assignments <- experiment(vector_of_probs)
#assigns the letters T or C, based on if the subject is in
#the treatment or control group
letter = ifelse( assignments$Treatment == 1, 84, 67)
plot(assignments$Score, assignments$Treatment,
     pch=letter, ylim=c(-0.5,1.5), yaxt = "n",
     xlab='Propensity Score', ylab='')
axis(side = 2, at = c(0,1), labels = c('Control', 'Treatment'))
abline(v=0.55, col='red', lty=2)
