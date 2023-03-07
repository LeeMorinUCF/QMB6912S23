##################################################
#
# QMB 6912 Capstone Project in Business Analytics
# PMSM-BA program
#
# Examples of Regression Models
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# January 21, 2023
#
##################################################
#
# Sample code for the problem sets in the course QMB 6912,
# Capstone Project in Business Analytics, for the PMSM-BA
# program.
# Tractor_Nonlinearity gives examples of OLS regression models
#   by considering a number of different model specifications.
# In this example, the model specification choices
#   use a parametric form to account to account for the nonlinearity.
#
# Dependencies:
#   Libraries xtable and texreg
#   to print tables of regression results.
#
##################################################


##################################################
# Preparing the Workspace
##################################################


# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory, if running interactively.
# wd_path <- '~/GitHub/QMB6912S23/demo_06/Tractor_Nonlinearity'
# setwd(wd_path)


# Set data directory.
data_dir <- 'Data'

# Set directory for storing figures.
fig_dir <- 'Figures'

# Set directory for storing tables.
tab_dir <- 'Tables'


##################################################
# Load libraries
##################################################

# Libraries to print tables of regression results.
library(xtable)
library(texreg)



##################################################
# Loading the Data
##################################################

in_file_name <- sprintf('%s/%s', data_dir, 'TRACTOR7.csv')
tractor_sales <- read.csv(file = in_file_name)

# Inspect the contents.
print('Summary of tractor_sales Dataset:')
print(summary(tractor_sales))

# Make sure there are no problems with the data.


##################################################
# Data Preparation
##################################################



# Take logs to bring outliers closer to the others.
tractor_sales[, 'log_saleprice'] <- log(tractor_sales[, 'saleprice'])



# Create categories for the horsepower variable.
summary(tractor_sales[, 'horsepower'])
# Collect observations into groups with similar horsepower.
tractor_sales[, 'hp_cat'] <- NA
sel_obs <- tractor_sales[, 'horsepower'] < 50
tractor_sales[sel_obs, 'hp_cat'] <- '0-50'
sel_obs <- tractor_sales[, 'horsepower'] >= 50 & 
  tractor_sales[, 'horsepower'] < 100
tractor_sales[sel_obs, 'hp_cat'] <- '50-100'
sel_obs <- tractor_sales[, 'horsepower'] >= 100 & 
  tractor_sales[, 'horsepower'] < 150
tractor_sales[sel_obs, 'hp_cat'] <- '100-150'
sel_obs <- tractor_sales[, 'horsepower'] >= 150 & 
  tractor_sales[, 'horsepower'] < 200
tractor_sales[sel_obs, 'hp_cat'] <- '150-200'
sel_obs <- tractor_sales[, 'horsepower'] >= 200 & 
  tractor_sales[, 'horsepower'] < 250
tractor_sales[sel_obs, 'hp_cat'] <- '200-250'
sel_obs <- tractor_sales[, 'horsepower'] >= 250
tractor_sales[sel_obs, 'hp_cat'] <- '250+'

# Set it as a factor to keep the variables in order.

tractor_sales[, 'hp_cat'] <- factor(tractor_sales[, 'hp_cat'], 
                                    levels = c('0-50', '50-100', 
                                               '100-150', '150-200', 
                                               '200-250', '250+'))

table(tractor_sales[, 'hp_cat'], useNA = 'ifany')  




##################################################
# Estimating a Regression Model
# Best reduced model from a strictly linear specification.
# Linear model for log of dollar sale price
# Omit engine hours, transmission type,
# seasonal indicators and John Deere indicator.
##################################################


# Estimate a regression model.
lm_model_best_lin <- lm(data = tractor_sales,
                     formula = log_saleprice ~ horsepower + age + 
                       diesel + fwd)

# Output the results to screen.
print(summary(lm_model_best_lin))



# Suppose for a moment that the horsepower variable were measured
# as a categorical variable.



# Estimate a regression model.
lm_model_hp_cat <- lm(data = tractor_sales,
                        formula = log_saleprice ~ 
                          horsepower + 
                          hp_cat + 
                          age + 
                          diesel + fwd)

# Output the results to screen.
print(summary(lm_model_hp_cat))


# Notice that the 50-100-horsepower group is 40% more valuable
# than 50 or less.
# The relationship levels off between 100 and 250 horsepower.
# Tractors with more than 250 horsepower are not statistically
# worth more than those with 0-50 horsepower, all else equal.
# This suggests a nonlinear relationship between horsepower and
# the tractor price.


# Since we have horsepower measured as a continuous variable, we can do better.



# Print the output to a LaTeX file.
tab_file_name <- 'hp_cat.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(list(lm_model_best_lin, 
            lm_model_hp_cat),
       file = out_file_name,
       label = 'tab:hp_cat',
       caption = "Log. of Tractor Prices")





##################################################
# Estimating a Regression Model
# Model 1: Linear model for dollar sale price
# All variables included (with linear specification).
##################################################

# Estimate a regression model.
lm_model_all_lin <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + age + enghours +
                   diesel + fwd + manual + johndeere +
                   spring + summer + winter + 
                   cab)

# Output the results to screen.
print(summary(lm_model_all_lin))




##################################################
#
# Introduce a Nonlinear Functional Form
#
# Consider a polynomial functional form for horsepower.
# Idea: Horsepower improves performance up to a limit,
# then extra power does not add value, only consumes more fuel.
#
# 1. Generate the squared variable.
# 2. Hypothesize the signs.
# 3. Add the squared horsepower term to the regression equation.
# 4. Estimate the revised model.
# 5. Analyze the resulting estimates.
# 6. Make recommendation for the new model.
#
##################################################

# Create a variable squared_horsepower
# to investigate quadratic relationship of sale price to horsepower.
tractor_sales[, 'squared_horsepower'] <- tractor_sales[, 'horsepower']^2



##################################################
# Reconsider other variables dropped before
# Using this new functional form for horsepower
##################################################


# Estimate the regression model.
lm_model_hp_all <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual + johndeere + cab +
                   spring + summer + winter)

# Output the results to screen.
print(summary(lm_model_hp_all))


##################################################
# Estimating a Regression Model
# Model 7: Linear model for log of dollar sale price
# With quadratic form for horsepower
# Omit seasonal indicators
##################################################

# Estimate a regression model.
lm_model_hp_red_1 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_model_hp_red_1))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_sq_horse.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_all_lin, 
                lm_model_hp_all,
                lm_model_hp_red_1),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_sq_horse',
       caption = "Quadratic Models for Tractor Prices")



##################################################
#
# Exercise: Test exclusion of seasonal indicators
#   An example of joint hypothesis testing.
print("Test for exclusion of seasonal indicators")
#
# The unconstrained RSS is calculated from the model
# that includes seasonal indicators:
RSS_unconstrained <- sum(lm_model_hp_all$residuals^2)
print("RSS_unconstrained:")
print(RSS_unconstrained)
#
# The constrained RSS is calculated from the model
# that excludes seasonal indicators:
RSS_constrained <- sum(lm_model_hp_red_1$residuals^2)
print("RSS_constrained:")
print(RSS_constrained)
#
# Follow the approach for conducting the F-test.
# Do used tractor prices follow a seasonal pattern?
#
##################################################

# Need sample size and number of variables.

num_obs <- nrow(tractor_sales)
num_vars <- 13 # including the intercept.

# A test of three restrictions (one for each seasonal dummy).
num_restr <- 3

F_stat <- (RSS_constrained - RSS_unconstrained)/num_restr /
  RSS_unconstrained*(num_obs - num_vars)
print("F-statistic:")
print(F_stat)

# This value is less than 1, let alone the critical value
# of the F-statistic at any degrees of freedom or
# any conventional level of significance.

# Conclude that used tractor prices do not follow a seasonal pattern.

##################################################
# Estimating a Regression Model
# Linear model for log of dollar sale price
# Interact Slope Indicator for Diesel with Engine Hours
##################################################

# Estimate a regression model.
lm_model_hp_int_1 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours + diesel*enghours + # Note the added term.
                   diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_model_hp_int_1))

# Does an additional hour of use affect a diesel-powered tractor
# differently than a gasoline-powered tractor?

# No improvement in R-bar-squared.
# Slope coefficient not significant.
# Diesel and engine coefficients same sign but no longer significant.

# Conclude that used tractor prices do not change with use
# differently for the type of fuel.



##################################################
# Estimating a Regression Model
# Linear model for log of dollar sale price
# Interact Slope Indicator for Engine Hours
# with John Deere Indicator
##################################################


# Estimate a regression model.
lm_model_hp_int_2 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual +
                   johndeere + johndeere*enghours + # Note the added term.
                   cab)

# Output the results to screen.
print(summary(lm_model_hp_int_2))

# No evidence that John Deere tractors
# wear out differently.

##################################################
# Estimating a Regression Model
# Linear model for log of dollar sale price
# Interact Slope Indicator for Age
# with John Deere Indicator
##################################################


# Estimate a regression model.
lm_model_hp_int_3 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual +
                   johndeere + johndeere*age + # Note the added term.
                   cab)

# Output the results to screen.
print(summary(lm_model_hp_int_3))


# No evidence that John Deere tractors
# age differently.


##################################################
# Estimating a Regression Model
# Linear model for log of dollar sale price
# Interact Slope Indicator for Age
# with John Deere Indicator
##################################################


# Estimate a regression model.
lm_model_hp_int_4 <- lm(data = tractor_sales,
                  formula = log_saleprice ~ horsepower + squared_horsepower +
                    age + enghours +
                    diesel + fwd + manual +
                    johndeere + johndeere*horsepower + # Note the added term.
                    cab)

# Output the results to screen.
print(summary(lm_model_hp_int_4))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_interactions.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_hp_int_1,
                lm_model_hp_int_2,
                lm_model_hp_int_3,
                lm_model_hp_int_4),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_interactions',
       caption = "Regression Models for Tractor Prices with Interactions")




##################################################
# Estimating a Regression Model
# Linear model for log of dollar sale price
# Separate Model for John Deere Tractors
##################################################


# Estimate the full regression model.
lm_model_hp_JD_1 <- lm(data = tractor_sales[tractor_sales[, 'johndeere'] == 1, ],
                  formula = log_saleprice ~ horsepower + squared_horsepower +
                    age + enghours +
                    diesel + fwd + manual +
                    cab)

# Output the results to screen.
print(summary(lm_model_hp_JD_1))

# Estimate a reduced regression model.
lm_model_hp_JD_2 <- lm(data = tractor_sales[tractor_sales[, 'johndeere'] == 1, ],
                  formula = log_saleprice ~
                    horsepower + squared_horsepower +
                    age + enghours +
                    # diesel +
                    # fwd +
                    # manual +
                    cab)

# Output the results to screen.
print(summary(lm_model_hp_JD_2))


##################################################
# Estimating a Regression Model
# Linear model for log of dollar sale price
# Separate Model for Tractors other than John Deere
##################################################


# Estimate the full regression model.
lm_model_hp_other_1 <- lm(data = tractor_sales[tractor_sales[, 'johndeere'] == 0, ],
                  formula = log_saleprice ~ horsepower + squared_horsepower +
                    age + enghours +
                    diesel + fwd + manual +
                    cab)

# Output the results to screen.
print(summary(lm_model_hp_other_1))


# Estimate a reduced regression model.
lm_model_hp_other_2 <- lm(data = tractor_sales[tractor_sales[, 'johndeere'] == 0, ],
                  formula = log_saleprice ~
                    horsepower + squared_horsepower +
                    age + enghours +
                    # diesel +
                    fwd + manual +
                    cab)

# Output the results to screen.
print(summary(lm_model_hp_other_2))



# Print the output to a LaTeX file.
tab_file_name <- 'reg_johndeere.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_hp_red_1,
                lm_model_hp_JD_1,
                lm_model_hp_JD_2,
                lm_model_hp_other_1,
                lm_model_hp_other_2),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_johndeere',
       caption = "Separate Partially Nonlinear Models by Brand")


##################################################
#
# Exercise: Test for separate coefficients by brand
#   An example of joint hypothesis testing.
print("Test for separate coefficients by brand")
#
# The unconstrained RSS is calculated from the models
# estimated separately by brand:
RSS_unconstrained <- sum(lm_model_hp_JD_2$residuals^2) +
  sum(lm_model_hp_other_2$residuals^2)
print("RSS_unconstrained:")
print(RSS_unconstrained)
#
# The constrained RSS is calculated from the model
# that includes only the John Deere indicator:
RSS_constrained <- sum(lm_model_hp_red_1$residuals^2)
print("RSS_constrained:")
print(RSS_constrained)
#
# Follow the approach for conducting the F-test.
# Do used tractor prices follow a separate pricing rule by brand?
#
##################################################

# Need sample size and number of variables.

num_obs <- nrow(tractor_sales)

# Total number of variables in both models.
summary(lm_model_hp_JD_2) # 6 parameters.
summary(lm_model_hp_other_2) # 8 parameters.
num_vars <- 14


# Compare with the single model on the full sample.
summary(lm_model_hp_red_1) # 10 parameters.

# A test of eight restrictions
# (one for each variable minus the interaction).
num_restr <- 14 - 10

F_stat <- (RSS_constrained - RSS_unconstrained)/num_restr /
  RSS_unconstrained*(num_obs - num_vars)
print("F-statistic:")
print(F_stat)

# This value is less than 1, let alone the critical value
# of the F-statistic at any degrees of freedom or
# any conventional level of significance.

# Conclude that used tractor prices do not follow a seasonal pattern.




##################################################
# End
##################################################
