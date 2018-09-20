# Data import & cleaning
library(readr)
dm1 <- read_csv("Google_Drive/School/Minerva/Classes/2018:2019/CS112/Assignments/DM1 - Drivetrain Approach/dm1_drivetrain.csv", 
                             col_types = cols(approval.date = col_date(format = "%d-%b-%y"), 
                                          cumulative.disbursements = col_double(), 
                                          implementation.start.date = col_date(format = "%d-%b-%y"), 
                                          original.completion.date = col_date(format = "%d-%b-%y"), 
                                          revised.completion.date = col_date(format = "%d-%b-%y"),
                                          undisbursed.amount = col_double(),
                                          project.type = col_factor(levels = NULL),
                                          status = col_factor(levels = NULL)))
# dm1$success.rating[is.na(dm1$success.rating)] <- 0
summary(dm1)
str(dm1)
plot(dm1$project.budget, dm1$approval.date)

# Determining which questions to answer
# HAC -> 813
set.seed(813)
my_questions <- sort(sample(c(1:10), 3, replace = FALSE))
my_questions #2, 7 & 9

# Removing data before 1/1/1995 & after 1/1/2017
dm1 <- dm1[(dm1$approval.date < "2017-01-01" & dm1$approval.date > "1995-01-01"),]

# Question 2 - What variables are measured for each project?
str(dm1)

# Question 7 - What does R’s “quantile” function tell you about the distributions of project budgets?
quantile(dm1$project.budget)

# Question 9 - Approximately what fraction of projects get assessed at project completion?
finished <- dm1[(dm1$status == "FINANCIALLY COMPLETED"),]
finished_assessed <- dm1[(!is.na(dm1$success.rating) & dm1$status == "FINANCIALLY COMPLETED"),]
nrow(finished_assessed) / nrow(finished)
