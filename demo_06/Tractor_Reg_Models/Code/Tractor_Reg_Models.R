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
# Tractor_Reg_Models gives examples of OLS regression models
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
# wd_path <- '~/GitHub/QMB6912S23/demo_06/Tractor_Reg_Models'
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
lm_model_price_all <- lm(data = tractor_sales,
                  formula = saleprice ~ horsepower + age + enghours +
                    diesel + fwd + manual + johndeere +
                    spring + summer + winter)

# Output the results to screen.
print(summary(lm_model_price_all))



##################################################
# Estimating a Regression Model
# Model 2: Linear model for log of dollar sale price
##################################################

# Estimate a regression model.
lm_model_log_all <- lm(data = tractor_sales,
                      formula = log_saleprice ~ horsepower + age + enghours +
                        diesel + fwd + manual + johndeere +
                        spring + summer + winter)

# Output the results to screen.
print(summary(lm_model_log_all))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_price_w_log.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(list(lm_model_price_all, lm_model_log_all),
       file = out_file_name,
       digits = 5,
       label = 'tab:reg_price_w_log',
       caption = "Linear and Logarithmic Models of Tractor Prices")



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
# Omit only the transmission type
# (keep seasonal indicators to omit one at a time)
##################################################


# Estimate a regression model.
lm_model_red_1 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + age + enghours +
                   diesel + fwd + johndeere +
                   spring + summer + winter)

# Output the results to screen.
print(summary(lm_model_red_1))




##################################################
# Estimating a Regression Model
# Model 4: Linear model for log of dollar sale price
# Omit seasonal indicators and transmission type
##################################################

# Estimate a regression model.
lm_model_red_2 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + age + enghours +
                   diesel + fwd + johndeere)

# Output the results to screen.
print(summary(lm_model_red_2))


##################################################
##################################################
# 
# Note that three seasonal indicators were dropped 
# in a single change. 
# This decision can be made better with a joint hypothesis test. 
#
# Test exclusion of seasonal indicators
# in a joint hypothesis test with an F-test.
print("Test for exclusion of seasonal indicators")
#
# The unconstrained RSS is calculated from the model
# that includes seasonal indicators:
RSS_unconstrained <- sum(lm_model_red_1$residuals^2)
print("RSS_unconstrained:")
print(RSS_unconstrained)
#
# The constrained RSS is calculated from the model
# that excludes seasonal indicators:
RSS_constrained <- sum(lm_model_red_2$residuals^2)
print("RSS_constrained:")
print(RSS_constrained)
#
# Follow the approach for conducting the F-test.
# Do used tractor prices follow a seasonal pattern?
#
##################################################

# Need sample size and number of variables.

num_obs <- nrow(tractor_sales)

# Check the number of parameters in the restricted model.
print(summary(lm_model_red_2)) # 7, without the three seasonal indicators..
print(summary(lm_model_red_1)) # 10, including the seasonal indicators.
num_vars <- 10

# A test of three restrictions (one for each seasonal dummy).
num_restr <- 3

F_stat <- (RSS_constrained - RSS_unconstrained)/num_restr /
  RSS_unconstrained*(num_obs - num_vars)
print("F-statistic:")
print(F_stat)

# This value is slightly more than 1, which is below the critical value
# of the F-statistic at any degrees of freedom or
# any conventional level of significance.

# Conclude that used tractor prices do not follow a seasonal pattern.

##################################################
##################################################


# Continue testing other exclusions.


##################################################
# Estimating a Regression Model
# Model 5: Linear model for log of dollar sale price
# Omit engine hours, transmission type
# and seasonal indicators.
##################################################


# Estimate a regression model.
lm_model_red_3 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + age + # enghours +
                   diesel + fwd + johndeere)

# Output the results to screen.
print(summary(lm_model_red_3))

##################################################
# Estimating a Regression Model
# Model 6: Linear model for log of dollar sale price
# Omit engine hours, transmission type,
# seasonal indicators and John Deere indicator.
##################################################


# Estimate a regression model.
lm_model_red_4 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + age + 
                   diesel + fwd)

# Output the results to screen.
print(summary(lm_model_red_4))
# Now all variables are significant;
# however, the John Deere indicator has been dropped. 



# Print the output to a LaTeX file.
tab_file_name <- 'reg_reduction.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_log_all,
                lm_model_red_1,
                lm_model_red_2,
                lm_model_red_3,
                lm_model_red_4),
       file = out_file_name,
       digits = 4,
       label = 'tab:reg_reduction',
       caption = "Models for the Log. of Tractor Prices")







##################################################
# Estimating a Regression Model
# Model 7: Linear model for log of dollar sale price
# Interact Slope Indicator for horsepower
# with John Deere Indicator
##################################################

# Is an extra unit of horsepower equally valuable for John Deere
# tractors vs the other brands?

# Estimate a regression model.
lm_model_int_1 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower +
                   age + enghours +
                   diesel + fwd + manual +
                   johndeere + johndeere*horsepower + # Note the added term.
                   cab)

# Output the results to screen.
print(summary(lm_model_int_1))



##################################################
# Estimating a Regression Model
# Model 8: Linear model for log of dollar sale price
# Interact Slope Indicator for Engine Hours
# with John Deere Indicator
##################################################


# Estimate a regression model.
lm_model_int_2 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower +
                   age + enghours +
                   diesel + fwd + manual +
                   johndeere + johndeere*enghours + # Note the added term.
                   cab)

# Output the results to screen.
print(summary(lm_model_int_2))

# No evidence that John Deere tractors
# wear out differently.

##################################################
# Estimating a Regression Model
# Model 9: Linear model for log of dollar sale price
# Interact Slope Indicator for Age
# with John Deere Indicator
##################################################


# Estimate a regression model.
lm_model_int_3 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower +
                   age + enghours +
                   diesel + fwd + manual +
                   johndeere + johndeere*age + # Note the added term.
                   cab)

# Output the results to screen.
print(summary(lm_model_int_3))


# No evidence that John Deere tractors
# age differently.



##################################################
# Estimating a Regression Model
# Model 10: Linear model for log of dollar sale price
# Interact Intercept Indicator for Manual Transmission
# with John Deere Indicator
##################################################

# The same test can be done for indicator variables
# to determine if the intercept changes with an indicator
# under a John Deere model.


# Estimate a regression model.
lm_model_int_4 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower +
                   age + enghours +
                   diesel + fwd + manual +
                   johndeere + johndeere*manual + # Note the added term.
                   cab)

# Output the results to screen.
print(summary(lm_model_int_4))

# Not very strong evidence that manual transmission has a different value
# for John Deere tractors.
# Check the others.

# None of the interactions were significant, so the best model so far is the
# one with only the John Deere indicator.


# Print the output to a LaTeX file.
tab_file_name <- 'reg_interactions.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_int_1,
                lm_model_int_2,
                lm_model_int_3,
                lm_model_int_4),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_interactions',
       caption = "Regression Models for Tractor Prices")




##################################################
# Estimating a Regression Model
# Models 11-13: Linear model for log of dollar sale price
# Separate Model for John Deere Tractors
##################################################


# Estimate the full regression model.
lm_model_JD_1 <- lm(data = tractor_sales[tractor_sales[, 'johndeere'] == 1, ],
                  formula = log_saleprice ~ horsepower +
                    age + enghours +
                    diesel + fwd + manual +
                    cab)

# Output the results to screen.
print(summary(lm_model_JD_1))

# Estimate a reduced regression model.
lm_model_JD_2 <- lm(data = tractor_sales[tractor_sales[, 'johndeere'] == 1, ],
                    formula = log_saleprice ~
                      horsepower +
                      age + enghours +
                      # diesel +
                      # fwd +
                      # manual +
                      cab)

# Output the results to screen.
print(summary(lm_model_JD_2))


# Estimate a reduced regression model.
lm_model_JD_3 <- lm(data = tractor_sales[tractor_sales[, 'johndeere'] == 1, ],
                    formula = log_saleprice ~
                      horsepower +
                      age + 
                      # enghours +
                      # diesel +
                      # fwd +
                      # manual +
                      cab)

# Output the results to screen.
print(summary(lm_model_JD_3))



# Doesn't look promising, since most of the variables are now insignificant.
# However, some differences may exist for the variables that remain.


##################################################
# Estimating a Regression Model
# Models 14-16: Linear model for log of dollar sale price
# Separate Model for Tractors other than John Deere
##################################################


# Estimate the full regression model.
lm_model_other_1 <- lm(data = tractor_sales[tractor_sales[, 'johndeere'] == 0, ],
                  formula = log_saleprice ~ horsepower +
                    age + enghours +
                    diesel + fwd + manual +
                    cab)

# Output the results to screen.
print(summary(lm_model_other_1))


# Diesel indicator is marginal,
# Estimate a reduced regression model.
lm_model_other_2 <- lm(data = tractor_sales[tractor_sales[, 'johndeere'] == 0, ],
                  formula = log_saleprice ~
                    horsepower +
                    age + enghours +
                    # diesel +
                    fwd + manual +
                    cab)

# Output the results to screen.
print(summary(lm_model_other_2))
# Diesel indicator should stay in.



# Print the output to a LaTeX file.
tab_file_name <- 'reg_johndeere.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_red_4,
                lm_model_JD_1,
                lm_model_JD_2,
                lm_model_JD_3,
                lm_model_other_1,
                lm_model_other_2),
       fontsize = 'footnotesize', # To display more columns.
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
# Take the best models estimated above.

summary(lm_model_JD_3)
summary(lm_model_other_1)


# Calculate the Residual Sum of Squares from both subsamples together.
RSS_unconstrained <- sum(lm_model_JD_3$residuals^2) +
  sum(lm_model_other_1$residuals^2)
print("RSS_unconstrained:")
print(RSS_unconstrained)
#

# The constrained RSS is calculated from the model
# that includes all observations.
summary(lm_model_red_4)

RSS_constrained <- sum(lm_model_red_4$residuals^2)
print("RSS_constrained:")
print(RSS_constrained)
#
# Follow the approach for conducting the F-test.
# Do used tractor prices follow a seasonal pattern?
#
##################################################

# Need sample size and number of variables.

num_obs <- nrow(tractor_sales)


# Count the number of parameters in the model for the full sample.
summary(lm_model_JD_3) # 4 parameters in reduced model for JD.
summary(lm_model_other_1) # 8 parameters in full model for other tractors.
num_vars <- 2*8 # If the same model with 5 parameters (including constant).
num_vars <- 12


# Count the number of additional parameters compared to the restricted model
# for the full sample.
summary(lm_model_red_4) # 5 parameters estimated with full sample.
num_restr <- 12 - 5

F_stat <- (RSS_constrained - RSS_unconstrained)/num_restr /
  RSS_unconstrained*(num_obs - num_vars)
print("F-statistic:")
print(F_stat)

# This is a high value
# compared to 
# the critical value
# of the F-statistic 
# at 7 and 12 degrees of freedom and
# conventional levels of significance.

# Conclude that used tractor prices should be modeled separately...


# ...so far. 




##################################################
# End
##################################################
