##### Q1 Politics #######
## PART 1: Calculate chi-squared testv stat by hand.
# Steps taken as follows...
# 1) Take 'not stopped' cells from upper and lower class rows to caluclate t stat
# 2) Observed value for upper class = 14. Observed value for lower class = 7
# 3) Calculate expected value for upper class first using function: 27/42*21 = fe 13.5
# 4) Calculate expected value for lower class 'not stopped' cell:  15/42*21 = fe 7.5
# 5) Use function to calculate t stat: 14 - 13.5/13.5 squared + 7 - 7.5/7.5 squared = 1.37 = 4.44 = 5.81
# ANS = 5.81

# PART 2: Calculate p-value from t-stat. Interpret it. Use pt() function
2*pt(5.81, df = 2, lower.tail = FALSE)
### ANS = 0.03

# PART 3: Calculate standardized residuals for each cell and place in table.
# Use formula for each cell to get standardized residuals.
# ANS for Upper Class = O.76, -3.32, 2.82
# ANS for Lower Class = -0.57, 2.46, -2.11

# PART 4: How Might the Standardized Rediduals Help Me Interpret Results?
# Standardised residuals differentiate expected and oberved values. Expected values come from the null hypothesis.
# They are important to interpretation of data by showing how important or unimportant each cell is to the overall result (i.e. how much a cell impacts a statistic  in a dataset).

###### Q2 Economics #####
# Read the csv file and name it economic_data
economic_data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
economic_data

### PART 1: State Null and Alternative Hypothesis. Interested only in reserved and water variables as we mean to test the effect of the reservation policy on new water facilities.
## Subset data to just reserved and water variables
reserved <- economic_data[, 3] 
reserved    
water <- economic_data[, 6]
water
# Find avg no of new water services to make data assumption and form null and alt hypothesis
mean(water) # 17.84
sd(water) # SD is 33.68, quite high.
# Null Hypothesis = The number of new water facilities will be less than 17.84 in regions with the reservation policy
# Alt Hypothesis = The no of new water facilities will be greater than 17.84 in regions where the reservation policy is introduced.

### Run regression using lm()
reserved_water <- lm(water ~ reserved)
reserved_water # Intercept =14.74, slope = 9.25
summary(reserved_water)
# Find Coefficient Estimate. ANS = 14.74 (see above)
# Less bthan 17.84. The null hyothesis is correct. 

      ##### Q3 Biology #####
## PART ONE: Obtain summary Stats on Lifespan
# Import fruitfly.csv
data <- read.csv("http://stat2.org/datasets/FruitFlies.csv")
data
# Conduct Summary Statistics on longevity incl. mean, sd, etc.
longevity <- data[, 4]
mean(longevity)
sd(longevity)
# Mean longevity = 57.44 days, sd = 17.56, sample size = 125

# PART 2: Plot Lifespan and Thorax and Comment Upon Relation. Find Correlation Coefficient
lifespan_thorax <- data[, 5, 4]
lifespan_thorax
plot(lifespan_thorax)
thorax <- data[, 5]
# Find correlation coefficient. ANS = 0.64
cor.test(longevity, thorax)
# Corrleation coefficient 0.64 close enough to 1 to have some evidence of positive relationship between thorax and lifespan.

# PART 3: Use lm() function to regress longevity on thorax. Find slope.
longthor_regression <- lm(longevity ~ thorax)
summary(longthor_regression)
# Intercept = -61.05. Slope for line = 144.33. Slope greater than 0. Therefore, there is a positive relationship between x and y variables.

### PART 4: Run and interpet significance test on lifesoan and thorax
# Use summary() function of longthor_regression to find p.value
summary(longthor_regression)
# P value less than 0.5. Relationship significant between lifespan and thorax.

### PART 5: Find 90 Confidence Interval
# Use summary() function to gather info to get 90 confidence interval...
# Slope = 144.33. Standard error of slope = 15.77. Find t stat to calculate by hand
# Use confint() function instead. Faster and more efficient.
confint(longthor_regression, level = 0.90)
# ANS = 118.20 and 170.47

## PART 6: Use predict function. 58th variable in thorax = 0.8mm. Subset thorax to 58th variable and use predict function
new_thorax <- thorax[58]
class(longthor_regression)
predict(longthor_regression, newdata = new_thorax)
