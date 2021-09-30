#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2021/problem_sets/PS1")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# Find a 90% Confidence Interval (CI) for the avergage IQ score of the entire school/population from smaple data y.
# First, sort the data and calculate mean, standard deviation and sample size.
y <- sort(y)
y_mean <- mean(y)
y_mean
y_sd <- sd(y)
y_sd
y_n <- length(y)
y_n
# Sample mean of IQ scores (y_mean) equals 98.44, standard deviation equals 13.09287 and sample size equals 25.
# Find z score using qnorm function. ANS equals 1.644854 and -1.644854
qnorm(.95)
qnorm(.05)
# Use function to find 90% CI. ANS = 4.307172
conf_int <- 1.644854*13.09287/sqrt(25)
conf_int
# Add and subtract 4.307172 from the sample mean to find 90% CI.
y_mean - conf_int
y_mean + conf_int
# 90% CI for the population average IQ score is between 94.13283 and 102.7472 range. 

##### Conduct Hypothesis Test on Whether School/Population average IQ is higher than national average (IQ 100)
# 5 steps: 1) Data assumptions, 2) Null and alt hypothesis, 3) Calculate T-stat, 4) Calculate P-value, 5) Conclusion

# 1) Data Assumptions: mean = 98.44, sd = 13.09287, sample size = 25, standord error = 2.618575, level of significance = 0.05. 
# 2) Forulate null and alt hypotheses. 
# Null Hypothesis: The avg IQ score of the school will be greater than or equal to 100.
# Alt Hypothesis: The avg IQ score of the school will be less than 100. 
# 3) Calculate test statistic by subtracting avg IQ nationally from sample avg IQ. ANS = -1.56
national_IQ_avg <- 100
Test_stat <- y_mean - national_IQ_avg
Test_stat
# Calculate P-value using pnorm function. P_value = 0.1187599
p_value <- 2*pnorm(-abs(-1.56))
p_value
# P value is greater than level of significance. Null hypothesis is therefore rejected.
p_value > 0.05

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)

# Explore data and plot relationships between Y, X1, X2 and X3
# Y = output varibale. X1, 2 and 3 = input variables
# Plot correlation between Y and X1. Subset function
income_exp <- expenditure[, 2:3]
plot(income_exp)
# No significant relationship or correlation between Y (housing exp) and X1 (personal income). Data very dispersed. 
# Plot correlation between Y and X2
insec_finance_exp <- expenditure[, c(2, 4)]
plot(insec_finance_exp)
# No significant relationship or correlation between Y (housing exp) and X2 (financial insecurity). Data quite dispersed. 
# Plot correlation between Y and X3
urban_exp <- expenditure[, c(2, 5)]
plot(urban_exp)
# No significant relationship or correlation between Y (housing exp) and X2 (urban residents). Data quite dispersed. 

##### Plot relationship between Y and Region #### 
#### Which region has highest per capita expenditure on housing assistance?
region_exp <- expenditure[, c(2, 6)]
region_exp
plot(region_exp)
# Subset region_exp into sub-regions
northeast_exp <- region_exp[1:9, 1]
northcentral_exp <- region_exp[10:21, 1]
south_exp <- region_exp[22:37, 1]
north_exp <- region_exp[38:50, 1]
# Find the mean expenditure on housing assiatnce for each region. Calculate which is highest
mean(northeast_exp)
mean(northcentral_exp)
mean(south_exp)
mean(north_exp)
# Region 4 (the North) spent most on housing assistance at 88.30769, followed by Region 2, Region 1 and lastly Region 3

##### Plot relationship between Y and X1. 
#### Include new variable 'Region'. Use colors and symbols for each region
reg_income_exp <- expenditure[, c(2, 3, 6)]
plot(reg_income_exp)
# Rename Region Values from Integers 1:4 to 'Northeast', 'North-Central', etc
reg_income_exp["Region"] [reg_income_exp["Region"] == 1] <- "Northeast"
reg_income_exp["Region"] [reg_income_exp["Region"] == 2] <- "North-Central"
reg_income_exp["Region"] [reg_income_exp["Region"] == 3] <- "South"
reg_income_exp["Region"] [reg_income_exp["Region"] == 4] <- "West"
reg_income_exp
plot(reg_income_exp)
