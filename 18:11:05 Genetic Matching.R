#--------------------------------------------
#Code for data set generation taken from:
#https://gist.github.com/anonymous/21dcc19db6ed5f2fa472d8b1eaf1cbb7
#--------------------------------------------

# PART 1: Get the 2 RCT data sets...
# website with data is here: http://users.nber.org/~rdehejia/data/nswdata2.html

# For the original Lalonde data
# STEP 1: Download the two text files, nsw_control.txt and nsw_treated.txt

# STEP 2: Ensure that they are in R's working directory

# STEP 3: Read them into your R workspace
nsw_controls <- read.table("nsw_control.txt")
nsw_treated <- read.table("nsw_treated.txt")

# STEP 4: Bind the treated and controls together, into one data frame
nsw_data <- rbind(nsw_treated, nsw_controls)
head(nsw_data)
additional_column_to_label_data_set <- rep(c("Original Lalonde Sample"), length(nsw_data[,1]))
nsw_data <- cbind(additional_column_to_label_data_set, nsw_data)
names(nsw_data) <-  c("data_id", "treat", "age", "educ", "black", "hisp",
                      "married", "nodegr", "re75", "re78")

head(nsw_data)

### For Dehejia's version of the Lalonde Data 
### I show you how to deal with files in STATA format below (very easy)
### Download the file: "nsw_dw.dta" and confirm it's in R's working directory
library(foreign)
DW_data <- read.dta("nsw_dw.dta")

head(DW_data)

###### PART II: CREATE THE FAKE OBSERVATIONAL DATA SETS

### Now to create the 2 simulated observational data sets that each combine the
### treatment group from the data sets above, with CPS-1 survey data

### A. ### First with the original Lalonde RCT sample

# Step 1: download "cps_controls.dta" and make sure it's in R's working directory

# Step 2: read in cps_controls.dta and confirm it has the same structure
cps_controls <- read.dta("cps_controls.dta")
head(cps_controls)

# notice that the columns of cps_controls and nsw_data are different:  
# nsw_data lacks the re74 column... we have to make the columns consistent b4 rbinding them
names(nsw_data)
names(cps_controls)

cps_controls_without_re74 <- cps_controls[,-9]
names(cps_controls_without_re74) <- names(nsw_data)

# Step 3: erase the RCT experiment's control group data from the Lalonde ("nsw_data") data set
nsw_data_nocontrols <- nsw_data[-which(nsw_data$treat == 0),]

# Step 4: rbind the nsw_data_nocontrols and the cps_controls together
nsw_treated_data_with_CPS <- rbind(nsw_data_nocontrols, cps_controls_without_re74)

### B. Second with Dehejia's experimental sample, which includes re74... 
###    in other words, 2 years of pre-treatment earnings -- Dehejia thought it was necessary
###    to control for more than 1 year of pre-treatment earnings...

# NEXT, make sure cps_controls has the same column names as Dehejia's experiment's data set
cps_controls_new_names <- cps_controls
names(cps_controls_new_names) <- names(DW_data)

# Step 3: erase the RCT experiment's control group data from Dehejia's ("nsw_data") data set
DW_data_nocontrols <- DW_data[-which(DW_data$treat == 0),]

# Step 4: rbind the nsw_data_nocontrols and the cps_controls together
DW_treated_data_with_CPS <- rbind(DW_data_nocontrols, cps_controls_new_names)

########## CONCLUSION
# you now have 4 data sets--
# 2 derived from RCTs, 
# 2 'hybrids' with treated units from each RCT and control units from the CPS survey
# nsw_data(_nocontrols), nsw_treated_data_with_CPS, DW_data(_nocontrols) & DW_treated_data_with_CPS


#------------------------------------------
#Difference of means in both samples
#------------------------------------------
control_mean_nsw <- mean(nsw_data$re78[nsw_data$treat == 0])
treat_mean_nsw <- mean(nsw_data_nocontrols$re78)
diff_mean_nsw <- treat_mean_nsw - control_mean_nsw
# diff_mean_nsw = 886.3037

control_mean_DW <- mean(DW_data$re78[DW_data$treat == 0])
treat_mean_DW <- mean(DW_data_nocontrols$re78)
diff_mean_DW <- treat_mean_DW - control_mean_DW
#diff_mean_DW = 1794.342

#------------------------------------------
#Assessing difference with T-tests
#------------------------------------------


#------------------------------------------
#Matching the DW data set
#------------------------------------------
library(MASS)
library(Matching)
library(rgenoud)

?GenMatch #-> calls on:
?Match #and
?MatchBalance

head(DW_treated_data_with_CPS)
DW_CPS <- DW_treated_data_with_CPS

#The covariates we want to match on
X = cbind(DW_CPS$age, DW_CPS$education, DW_CPS$black,
          DW_CPS$hispanic, DW_CPS$married, DW_CPS$nodegree,
          DW_CPS$re74, DW_CPS$re75)
#The outcome variable
Y=DW_CPS$re78

#Let's call GenMatch() to find the optimal weight to give each
#covariate in 'X'

genout <- GenMatch(Tr=DW_CPS$treat, X=X, estimand="ATT", M=1,
                   pop.size=50, max.generations=5, wait.generations=2, replace = FALSE)

# estimating our causal effect of interest using the genout weights

mout <- Match(Y=Y, Tr=DW_CPS$treat, X=X, estimand="ATT", Weight.matrix=genout, M=1,
              replace = FALSE)
#---------------------------------------
# Estimate...  1507.5 
# compared to diff_mean_DW...  1794.342
#---------------------------------------
#                        
#Let's determine if balance has actually been obtained on the variables of interest
#                        
mb <- MatchBalance(DW_CPS$treat ~ DW_CPS$age + DW_CPS$education + DW_CPS$black +
                   DW_CPS$hispanic + DW_CPS$married + DW_CPS$nodegree +
                   DW_CPS$re74 + DW_CPS$re75,
                   match.out=mout, nboots=500)

summary(mout)


