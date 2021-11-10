##### PROBLEM SET 3 #########
# Read csv file
incumbents <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/incumbents_subset.csv", header=T)
incumbents

### Question 1 ####
# Part (a): Run regression between voteshare (outcome var) and difflog (explanatory var)
voteshare_difflog <- lm(incumbents$difflog ~ incumbents$voteshare)
summary(voteshare_difflog) # Intercept = -3.949, slop = 8.816

# Part (b): Make scatterplot and add regression line
plot(voteshare_difflog)

# Part (c): Save residuals of model in a seperate object
residuals1 <- residuals(voteshare_difflog) # Use residuals() function on voteshare_difflog
residuals1

# Part (d): Write the prediction equation

#### Question 2 #####
# Part (a): Run regression with output var presvote and explanatory var difflog
difflog_presvote <- lm(incumbents$difflog ~ incumbents$presvote)
difflog_presvote

# Part (b): Make scatterplot of difflog_presvote and add regression line
plot(difflog_presvote)
abline(difflog_presvote)

# Part (c): Save residuals of model in sperate object
residuals2 <- residuals(difflog_presvote)
residuals2

# Part (d): Write prediction equation

#### Question 3 ######
# Part (a): Run regression with outcome var presvote and explanatory var voteshare
presvote_voteshare <- lm(incumbents$presvote ~ incumbents$voteshare)
presvote_voteshare

# Part (b): Make scatterplot and add regression line
plot(presvote_voteshare)
abline(presvote_voteshare)

# Part (c): Write prediction equation

##### Question 4 #######
# Part (a): Run regression between outcome var is residuals1 and explanatory var is residuals2
comb_residuals <- lm(incumbents$residuals2 ~ incumbents$residuals1)

# Part (b): Make scatterplot of residuals and add regression line

# Part (c): Write prediction equation

##### Question 5 ######
# Part (a): Muti-Variate Regression - Run regression between outcome var voteshare and explanatory variables difflog and presvote

# Part (b): Make a scatterplot and add regression line

# Part (c): Write the prediction equation
