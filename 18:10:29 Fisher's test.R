city.names <- c("Sag", "Sio", "Bat", "Mid", 
                "Ox", "Low", "Yak", "Rich")


observed.turnout = c(17, 30, 13, 55, 26, 29, 48, 43)
observed.diffmeans <- mean(observed.turnout[c(2,4,6,8)]) - 
  mean(observed.turnout[c(1,3,5,7)])

print(observed.diffmeans)

foo <- data.frame(city.names, observed.turnout)

# Turnout Function
turnout <- function() {
  # Four coin flips, establishing random assignment
  assignment        <- foo[sample(1:2),]
  assignment[3:4,]  <- foo[sample(3:4),]
  assignment[5:6,]  <- foo[sample(5:6),]
  assignment[7:8,]  <- foo[sample(7:8),]
  
  treatment.group   <- assignment[c(1,3,5,7),]
  control.group     <- assignment[c(2,4,6,8),]
  
  return(mean(treatment.group[,2]) - mean(control.group[,2]))
}

# Iterating the Turnout Function
iter.RI <- function(iterations = 100000) {
  for (i in 1:iterations) 
  {storage.vector[i] <- turnout()
  }
  return(storage.vector)
}

storage.vector <- NULL
results <- iter.RI()

# Exploring the results
vvv <- quantile(results, prob = c(0.95))
length(unique(results))
hist(results)
plot(density(results))
abline(v = vvv, lwd = 2, col = "red")
