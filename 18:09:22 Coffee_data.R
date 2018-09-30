set.seed(42)

# Generating the data set 1.2
df_coffee <- data.frame(latte_price = integer(), espresso_price = integer(), chai_price = integer(), avg_rent = integer(), stringsAsFactors=FALSE)
df_coffee <- cbind(latte_price = runif(200, 2.5, 6), espresso_price = runif(200, 1, 4), chai_price = runif(200, 2.5, 7), avg_rent = runif(200, 0, 0))
df_coffee <- data.frame(df_coffee)
df_coffee$avg_rent <- df_coffee$latte_price * 124 + df_coffee$espresso_price * 88 + df_coffee$chai_price * 44 + rnorm(200, 0, 100)

# Saving the data 2.1
sub_index <- sample(c(1:200), 100)
training_data_coffee <- df_coffee[sub_index,]
testing_data_coffee <- df_coffee[-sub_index,]
write.csv(training_data_coffee, file = "training_data_coffee.csv")
write.csv(testing_data_coffee, file = "testing_data_coffee.csv")
# coffee_model <- lm(avg_rent ~ latte_price + espresso_price + chai_price, df_coffee)

# Calculating R2 of my own model 2.2
install.packages("Metrics")
library(Metrics)

coffee_model <- lm(avg_rent ~ latte_price + espresso_price + chai_price, training_data_coffee)
coffee_predictions <- predict.lm(coffee_model, testing_data_coffee)
R2_coffee <- 1 - (sum((testing_data_coffee$avg_rent-coffee_predictions )^2)/sum((testing_data_coffee$avg_rent-mean(testing_data_coffee$avg_rent))^2))
sqrt(mse(testing_data_coffee$avg_rent, coffee_predictions))
R2_coffee


# playing with Chris' model
library(readr)
training_data_happiness <- read_csv("Google_Drive/School/Minerva/Classes/2018:2019/CS112/1.2-2.2 Preclass/training_data_happiness.csv")
testing_data_happiness <- read_csv("Google_Drive/School/Minerva/Classes/2018:2019/CS112/1.2-2.2 Preclass/testing_data_happiness.csv")
happiness_model <- lm(happiness ~ sleep + exercise, training_data_happiness)

# stats in training set
happiness_predictions_intraining <- predict.lm(happiness_model, training_data_happiness)
R2_happiness_intraining <- 1 - (sum((training_data_happiness$happiness-happiness_predictions_intraining)^2)/sum((training_data_happiness$happiness-mean(training_data_happiness$happiness))^2))
sqrt(mse(training_data_happiness$happiness, happiness_predictions_intraining))
R2_happiness_intraining

# stats in testing set
happiness_predictions_intest <- predict.lm(happiness_model, testing_data_happiness)
R2_happiness_intest <- 1 - (sum((testing_data_happiness$happiness-happiness_predictions_intest)^2)/sum((testing_data_happiness$happiness-mean(testing_data_happiness$happiness))^2))
sqrt(mse(testing_data_happiness$happiness, happiness_predictions_intest))
R2_happiness_intest
print("RMSE Ratio training to test: ")
sqrt(mse(training_data_happiness$happiness, happiness_predictions_intraining))/sqrt(mse(testing_data_happiness$happiness, happiness_predictions_intest))

# Plotting some of this <3
library(arm)
sleep <- testing_data_happiness$sleep
exercise <- testing_data_happiness$exercise
happiness <- testing_data_happiness$happiness

colors <- ifelse(c(exercise, sleep) >= 3, "black", "grey" )
plot(c(exercise, sleep), c(happiness, happiness), xlab="Exercise & Sleep", ylab="Happiness", pch=20, col=colors)
curve(coef(happiness_model)[1] + coef(happiness_model)[2]*x, add=TRUE, col="black")
curve(coef(happiness_model)[1] + coef(happiness_model)[3]*x, add=TRUE, col="gray")