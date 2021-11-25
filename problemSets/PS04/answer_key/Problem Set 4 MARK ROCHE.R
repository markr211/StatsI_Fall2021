#### PROBLEM SET 4 - MARK ROCHE ######
install.packages(car)
library(car)
data(Prestige)
help(Prestige)

##### Question 1 ######

# (a): Create a new variable professional by recoding the variable type so that 
# professionals are coded as 1, and blue and white collar workers are coded as 0
data_prestige <- Prestige
# Rename the column 'type' as 'professionals'
names(data_prestige)[names(data_prestige) == 'type'] <- 'professionals'
# Write if/else loop to code professionals under this column as 1, 
# and blue and white collar workers as 0.
data_prestige$professionals <- ifelse(data_prestige$professionals %in% c("bc", 'wc') ,data_prestige$professionals<-0, data_prestige$professionals<-1)
data_prestige 
# (b): Run a linear model with prestige as an outcome, and income, professional 
# and the interaction of the two as predictors.
prestige_regression <- lm(prestige ~ professionals+income, data=data_prestige)
summary(prestige_regression)
plot(prestige_regression)
abline(prestige_regression)
# Intercept = 27.863, proffesion slope = 17.761, income slope = 0.002

# (c): Write the prediction equation based on the result.
# Equation = y = 27.863+17.761*x1+0.002*x2
# X1 and X2 are randomly chosen explanatory (or x) variables that can  predict y (or outcome values) 
# when they occur. 
# E.g. Take the first value for professionals in the dataset 'dataprestige' which equals 1 as 
# the x1 value and the first value for income which equals 12351 as the x2 value to get prestige 
# (or y value predicted when these x1 and x2 values occur)
y <- 27.863+17.761*1+0.002*12351
y 
# Predicted y (or Pineo-Porter prestige score) is 70.326 when income = 12351 and the person is a professional 
# (i.e. it holds 1 as value)

# (d): Interpret the coefficient for income.
# Coefficient = 0.02. Close to zero coefficient signals close to a null relationship between 
# income and prestige.

# (e): Interpret the coefficient for professional.
# Coefficient = 17.761. Indicates very strong relationship between one's profession and their prestige score. 

# (f): What is the effect of a $1,000 increase in income on 
# prestige score for professional occupations? 
# In other words, we are interested in the marginal effect of income when the variable 
# professional takes the value of 1. Calculate the change in yˆ associated with a $1,000 
# increase in income based on your answer for (c).
# Add 1000 income to income from (c) equation which equals 12351. 
12351 + 1000 # ANS = 13351
# Place in new equation instead of 12351 income
y1 <- 27.863+17.761*1+0.002*13351
y1 # Pineo-Porter prestige score = 72.326 when income is increased by 1000.
# This is compared with 70.326 from answer to (c). Therefore 1000 income increase associated
# with 2 point increase in prestige points. 
# Prestige therefore increases by a little bit if income is increased. This relates to the slope close
# to zero referred to in part (d). Income therefore likely weakly explains prestige in this model.

# (g): What is the effect of changing one’s occupations from non-professional to professional 
# when her income is $6,000? We are interested in the marginal effect of professional jobs 
# when the variable income takes the value of 6,000. Calculate the change in yˆ based on your 
# answer for (c).
y2 <- 27.863+17.761*1+0.002*6000
y2 # ANS = 57.624 prestige points 
27.863+17.761*0+0.002*6000 # ANS = 39.863 prestige points
# Subtract both prestige points to get effect of profession on prestige at same income rate of 6,000
57.624 - 39.863
# ANS = 17.761. This is the same value as the slope for professions.
# Therefore, even if they earn the same income of 6,000, a professional is likely to recieve 
# 17.761 more prestige points than blue and white collar workers. 
# Therefore, this indicates that the impact of profession is very large on prestige relative to income. 



###### Question 2 #####

# (a): Use the results from a linear regression to determine whether having these yard signs 
# in a precinct affects vote share (e.g., conduct a hypothesis test with α = .05).
# Yard signs is explanatory variable, vote share is outcome variable
# 1) Assumptions: Sample size n = 30, slope = 0.042, standard error = 0.016
# 2) Null and Alt Hypotheses: H0 = the effect of yard signs on vote share is null (i.e. the slope = 0), 
# HA = the effect of yard signs on vote share is present (i.e. the slope does not = O) 
# 3) Calculate test stat by dividing slope and null hyp for slope (or O) by standard error (i.e. 0.016)
0.042/0.016 # T stat = 2.625
# 4) P-value: Use T table
# N - 1df (i.e. 30-1) ANS = 19. T stat on row 19 of t table falls between 0.01 and 0.005. Therefore, 0.0075
# for one tail. Multiply by 2 for two-tail test = 0.05
# 5) Conclusions: p-value is = to level of significance 0.05. Therefore, null can be just about be rejected.
# We can determine that it is possible that the prescence of these yard signs within a precicint 
# may impact vote share. 

# (b): Use the results to determine whether being next to precincts with these yard signs affects 
# vote share (e.g., conduct a hypothesis test with α = .05).
# 1) Assumptions: Sample size n = 76, intercept = 0.042, standard error = 0.013
# 2) Null and Alt Hypotheses: H0 = no effect on vote share (slope = 0), HA = effect on vote share (i.e. slope does not
# = 0)
# 3) Test Stat: 
0.042/0.013 # T stat = 3.230769
# 4) P-value = 0.9991
# 5) Conclusions: P value greater than 0.05. Therefore, we cannot reject the null hypothesis. It cannot
# be determined that being adjecent to precincts using yard signs impacts vote share

# (c): Interpret the coefficient for the constant term substantively.
# The constant 0.302 here is the Y-intercept (where the line crosses the y axis). Standard error of intercept
# is 0.011. 
# It tells us that if lawn signs present within and adjacent to precincts are zero  (i.e.
# there are no yard signs), vote share = 0.302. Therefore, with no yard signs, 0.302 of vote share will go 
# to Ken Cuccinelli. 

# (d): Evaluate the model fit for this regression. What does this tell us about the importance of 
# yard signs versus other factors that are not modeled?
# Interpret Rsqaured 0.094 to evaluate model fit. Rsquared always between 0-100 percent
# 0.094 = 9.4%
# Closer to 100%, the more the model explains outcome variable variance
# Therefore, model explains only 9.4% of vote share  variance
# Indicates that other factors that are not modeled are likley  have a greater effect on vote share than
# yard signs. This is because 90.6% of vote share variance is not explained by yard signs.  
