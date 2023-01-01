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
# College of Business Administration
# University of Central Florida
#
# March 23, 2021
#
##################################################
#
# Sample code for the problem sets in the course QMB 6912,
# Capstone Project in Business Analytics, for the PMSM-BA
# program.
# Tractor_Model_Spec gives examples of OLS regression models
#   by considering a number of different model specifications.
# In this example, the model specification choices
#   have a parametric form.
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
# wd_path <- '~/GitHub/QMB6912S22/demo_07/Tractor_Reg_Models'
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

# First plot a histogram with the default options.
fig_file_name <- 'hist_price.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
hist(tractor_sales[, 'saleprice'],
     main = 'Relative Histogram of Tractor Prices',
     xlab = 'Price',
     probability = TRUE,
     col = 'red',
     breaks = 25)
dev.off()

# Notice that there are some very large values.
# Consider taking logs to bring outliers closer to the others.

tractor_sales[, 'log_saleprice'] <- log(tractor_sales[, 'saleprice'])

# Now plot the histogram for log of saleprice:

# Now plot the histogram for log of saleprice:
fig_file_name <- 'hist_log_price.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
hist(tractor_sales[, 'log_saleprice'],
     main = 'Histogram of the Logarithm of Tractor Prices',
     xlab = 'Logarithm of Price',
     probability = TRUE,
     col = 'red',
     breaks = 25)
dev.off()

# Much better behaved. Looks almost normal.

# So far it looks as if the log of saleprice
# is the more promising variable.
# Another approach to making this decision is
# to build the model and judge the validity of the results.


##################################################
# Estimating a Regression Model
# Model 1: Linear model for dollar sale price
##################################################

# Estimate a regression model.
lm_model_1 <- lm(data = tractor_sales,
                  formula = saleprice ~ horsepower + age + enghours +
                    diesel + fwd + manual + johndeere +
                    spring + summer + winter)

# Output the results to screen.
print(summary(lm_model_1))

# Print the output to a LaTeX file.
tab_file_name <- 'price_reg_1.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(lm_model_1,
       file = out_file_name,
       label = 'tab:price_reg_1',
       caption = "Dollar Value of Tractor Prices")


##################################################
# Estimating a Regression Model
# Model 2: Linear model for log of dollar sale price
##################################################

# Estimate a regression model.
lm_model_2 <- lm(data = tractor_sales,
                      formula = log_saleprice ~ horsepower + age + enghours +
                        diesel + fwd + manual + johndeere +
                        spring + summer + winter)

# Output the results to screen.
print(summary(lm_model_2))


# Print the output to a LaTeX file.
tab_file_name <- 'log_price_reg_2.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(lm_model_2,
       file = out_file_name,
       digits = 5,
       label = 'tab:log_price_reg_2',
       caption = "Logarithm of Tractor Prices")



##################################################
# Compare the distribution of variables
# across subsets by John Deere indicator.
##################################################


summary(tractor_sales[, 'saleprice'])

# See what a difference the John Deere label is worth:
summary(tractor_sales[tractor_sales[, 'johndeere'] == 1, 'saleprice'])
summary(tractor_sales[tractor_sales[, 'johndeere'] == 0, 'saleprice'])


##################################################
# Estimating a Regression Model
# Model 3: Linear model for log of dollar sale price
# Omit seasonal indicators
##################################################

# Estimate a regression model.
lm_model_3 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + age + enghours +
                   diesel + fwd + manual + johndeere)

# Output the results to screen.
print(summary(lm_model_3))


##################################################
# Estimating a Regression Model
# Model 4: Linear model for log of dollar sale price
# Omit seasonal indicators and transmission type
##################################################

# Estimate a regression model.
lm_model_4 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + age + enghours +
                   diesel + fwd + johndeere)

# Output the results to screen.
print(summary(lm_model_4))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_reduction.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_2,
                lm_model_3,
                lm_model_4),
       file = out_file_name,
       digits = 4,
       label = 'tab:reg_reduction',
       caption = "Models for the Log. of Tractor Prices")



##################################################
#
# Exercise:
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


# Estimate a regression model.
lm_model_5 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + johndeere)

# Output the results to screen.
print(summary(lm_model_5))






##################################################
# Reconsider other variables dropped before
# Using this new functional form for horsepower
##################################################


# Estimate the regression model.
lm_model_6 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual + johndeere + cab +
                   spring + summer + winter)

# Output the results to screen.
print(summary(lm_model_6))


##################################################
# Estimating a Regression Model
# Model 7: Linear model for log of dollar sale price
# With quadratic form for horsepower
# Omit seasonal indicators
##################################################

# Estimate a regression model.
lm_model_7 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_model_7))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_sq_horse.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_5,
                lm_model_6,
                lm_model_7),
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
RSS_unconstrained <- sum(lm_model_6$residuals^2)
print("RSS_unconstrained:")
print(RSS_unconstrained)
#
# The constrained RSS is calculated from the model
# that excludes seasonal indicators:
RSS_constrained <- sum(lm_model_7$residuals^2)
print("RSS_constrained:")
print(RSS_constrained)
#
# Follow the approach for conducting the F-test.
# Do used tractor prices follow a seasonal pattern?
#
##################################################

# Need sample size and number of variables.

num_obs <- nrow(tractor_sales)
num_vars <- 12

# A test of three restrictions (one for each seasonal dummy).
num_restr <- 3

F_stat <- (RSS_constrained - RSS_unconstrained)/num_restr /
  RSS_unconstrained*(num_obs - num_vars - 1)
print("F-statistic:")
print(F_stat)

# This value is less than 1, let alone the critical value
# of the F-statistic at any degrees of freedom or
# any conventional level of significance.

# Conclude that used tractor prices do not follow a seasonal pattern.

##################################################
# Estimating a Regression Model
# Model 8: Linear model for log of dollar sale price
# Interact Slope Indicator for Diesel with Engine Hours
##################################################

# Estimate a regression model.
lm_model_8 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours + diesel*enghours + # Note the added term.
                   diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_model_8))

# Does an additional hour of use affect a diesel-powered tractor
# differently than a gasoline-powered tractor?

# No improvement in R-bar-squared.
# Slope coefficient not significant.
# Diesel and engine coefficients same sign but no longer significant.

# Conclude that used tractor prices do not change with use
# differently for the type of fuel.



##################################################
# Estimating a Regression Model
# Model 9: Linear model for log of dollar sale price
# Interact Slope Indicator for Engine Hours
# with John Deere Indicator
##################################################


# Estimate a regression model.
lm_model_9 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual +
                   johndeere + johndeere*enghours + # Note the added term.
                   cab)

# Output the results to screen.
print(summary(lm_model_9))

# No evidence that John Deere tractors
# wear out differently.

##################################################
# Estimating a Regression Model
# Model 10: Linear model for log of dollar sale price
# Interact Slope Indicator for Age
# with John Deere Indicator
##################################################


# Estimate a regression model.
lm_model_10 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual +
                   johndeere + johndeere*age + # Note the added term.
                   cab)

# Output the results to screen.
print(summary(lm_model_10))


# No evidence that John Deere tractors
# age differently.


##################################################
# Estimating a Regression Model
# Model 11: Linear model for log of dollar sale price
# Interact Slope Indicator for Age
# with John Deere Indicator
##################################################


# Estimate a regression model.
lm_model_11 <- lm(data = tractor_sales,
                  formula = log_saleprice ~ horsepower + squared_horsepower +
                    age + enghours +
                    diesel + fwd + manual +
                    johndeere + johndeere*horsepower + # Note the added term.
                    cab)

# Output the results to screen.
print(summary(lm_model_11))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_interactions.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_8,
                lm_model_9,
                lm_model_10,
                lm_model_11),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_interactions',
       caption = "Regression Models for Tractor Prices")




##################################################
# Estimating a Regression Model
# Models 12-13: Linear model for log of dollar sale price
# Separate Model for John Deere Tractors
##################################################


# Estimate the full regression model.
lm_model_12 <- lm(data = tractor_sales[tractor_sales[, 'johndeere'] == 1, ],
                  formula = log_saleprice ~ horsepower + squared_horsepower +
                    age + enghours +
                    diesel + fwd + manual +
                    cab)

# Output the results to screen.
print(summary(lm_model_12))

# Estimate a reduced regression model.
lm_model_13 <- lm(data = tractor_sales[tractor_sales[, 'johndeere'] == 1, ],
                  formula = log_saleprice ~
                    horsepower + squared_horsepower +
                    age + enghours +
                    # diesel +
                    # fwd +
                    # manual +
                    cab)

# Output the results to screen.
print(summary(lm_model_13))


##################################################
# Estimating a Regression Model
# Models 14-15: Linear model for log of dollar sale price
# Separate Model for Tractors other than John Deere
##################################################


# Estimate the full regression model.
lm_model_14 <- lm(data = tractor_sales[tractor_sales[, 'johndeere'] == 0, ],
                  formula = log_saleprice ~ horsepower + squared_horsepower +
                    age + enghours +
                    diesel + fwd + manual +
                    cab)

# Output the results to screen.
print(summary(lm_model_14))


# Estimate a reduced regression model.
lm_model_15 <- lm(data = tractor_sales[tractor_sales[, 'johndeere'] == 0, ],
                  formula = log_saleprice ~
                    horsepower + squared_horsepower +
                    age + enghours +
                    # diesel +
                    fwd + manual +
                    cab)

# Output the results to screen.
print(summary(lm_model_15))



# Print the output to a LaTeX file.
tab_file_name <- 'reg_johndeere.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_7,
                lm_model_12,
                lm_model_13,
                lm_model_14,
                lm_model_15),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_johndeere',
       caption = "Separate Models by Brand")


##################################################
#
# Exercise: Test for separate coefficients by brand
#   An example of joint hypothesis testing.
print("Test for separate coefficients by brand")
#
# The unconstrained RSS is calculated from the models
# estimated separately by brand:
RSS_unconstrained <- sum(lm_model_12$residuals^2) +
  sum(lm_model_13$residuals^2)
print("RSS_unconstrained:")
print(RSS_unconstrained)
#
# The constrained RSS is calculated from the model
# that includes only the John Deere indicator:
RSS_constrained <- sum(lm_model_7$residuals^2)
print("RSS_constrained:")
print(RSS_constrained)
#
# Follow the approach for conducting the F-test.
# Do used tractor prices follow a seasonal pattern?
#
##################################################

# Need sample size and number of variables.

num_obs <- nrow(tractor_sales)
num_vars <- 2*9

# A test of eight restrictions
# (one for each variable minus the interaction).
num_restr <- 9 - 1

F_stat <- (RSS_constrained - RSS_unconstrained)/num_restr /
  RSS_unconstrained*(num_obs - num_vars - 1)
print("F-statistic:")
print(F_stat)

# This value is less than 1, let alone the critical value
# of the F-statistic at any degrees of freedom or
# any conventional level of significance.

# Conclude that used tractor prices do not follow a seasonal pattern.




##################################################
# End
##################################################
