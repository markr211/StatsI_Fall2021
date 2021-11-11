##### PROBLEM SET 3 - MARK ROCHE - ASDS - STATS1  #########
# Read csv file
incumbents <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/incumbents_subset.csv", header=T)
incumbents

### Question 1 ####
# Part (a): Run regression between voteshare (outcome var) and difflog (explanatory var)
voteshare_difflog <- lm(incumbents$voteshare ~ incumbents$difflog)
voteshare_difflog # Intercept = 0.58, slope = 0.04

# Part (b): Make scatterplot and add regression line
plot(voteshare_difflog)
abline(voteshare_difflog)
# Moderately positive/null relation  between variables

# Part (c): Save residuals of model in a seperate object
residuals1 <- residuals(voteshare_difflog) # Use residuals() function on voteshare_difflog
residuals1

# Part (d): Write the prediction equation
# Equation = intercept + slope multiplied by random x value
# E.g. use 15 as random x value
0.58 + 0.04*15
# Predicted value is 1.18

#### Question 2 #####
# Part (a): Run regression with output var presvote and explanatory var difflog
difflog_presvote <- lm(incumbents$presvote ~ incumbents$difflog)
difflog_presvote # Intercept = 0.51, slope = 0.02

# Part (b): Make scatterplot of difflog_presvote and add regression line
plot(difflog_presvote)
abline(difflog_presvote)
# Moderately postitve/null relation observed in graph

# Part (c): Save residuals of model in seperate object
residuals2 <- residuals(difflog_presvote)
residuals2

# Part (d): Write the prediction equation
difflog_presvote
# Equation = intercept + slope multiplied by random x value
# E.g. use 5 as random x value
0.51 + 0.02*5
# Predicted y value is 0.61

#### Question 3 ######
# Part (a): Run regression with outcome var presvote and explanatory var voteshare
presvote_voteshare <- lm(incumbents$presvote ~ incumbents$voteshare)
presvote_voteshare

# Part (b): Make scatterplot and add regression line
plot(presvote_voteshare)
abline(presvote_voteshare)
# Moderately positive/null relation obeserved in graph

# Part (c): Write the prediction equation
summary(presvote_voteshare)
# Equation is y = 0.2036 + 0.5304 * random x value
# X value = 9
0.2036 + 0.5304*9
# Predicted y value = 4.9772

##### Question 4 #######
# Part (a): Run regression between outcome var residuals1 and explanatory var residuals2
comb_residuals <- lm(residuals1 ~ residuals2)
comb_residuals # Intercept = 4.498, slope = 6.866

# Part (b): Make scatterplot of residuals and add regression line
plot(comb_residuals)
abline(comb_residuals)
# Strongly positive relation between residuals indicated by plot

# Part (c): Write prediction equation
# Add intercept and slope and multiply slope by random x value (i.e. residual2 value)
# X value = 20
4.498 + 6.866*20
# Predicted y value when x is 20 = 141.818

##### Question 5 ######
# Part (a): Multi-Variate Regression 
# Run regression between outcome var voteshare and explanatory variables difflog and presvote
voteshare_diff_presvote <- lm(voteshare ~ difflog+presvote, data=incumbents)
voteshare_diff_presvote # Intercept = 0.44864, slopes = 0.03554 and 0.25688
#  Make a scatterplot and add regression line to visualise regression
plot(voteshare_diff_presvote)
abline(voteshare_diff_presvote)

# Part (b): Write the prediction equation
# Use x values from sample. E.g. First difflog value 0.570 and first presvote value 0.527
0.459+0.036*0.570+0.257*0.527
# Predicted y value = 0.615

# Part (c): Explain result to prediction  result in Q4.




