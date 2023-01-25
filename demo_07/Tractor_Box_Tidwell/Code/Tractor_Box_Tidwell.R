##################################################
#
# QMB 6912 Capstone Project in Business Analytics
#
# Examples of Model Specfications
# with the Box-Tidwell Transformation
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
# Tractor_Box_Tidwell gives examples of additive
#   regression models augmented with a number of
#   different nonlinear model specifications,
#   using  Box-Tidwell transformations.
#
# Dependencies:
#   Libraries xtable and texreg
#   to print tables of regression results.
#   car for fitting models with Box-Tidwell transformations
#      Notice that the box.tidwell() function is deprecated.
#      The currently available function must be spelled
#      boxTidwell() with no dot and a capital T.

#
##################################################


##################################################
# Preparing the Workspace
##################################################


# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory, if running interactively.
# wd_path <- '~/GitHub/QMB6912S23/demo_07/Tractor_Box_Tidwell'
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

# Library car for fitting models with Box-Tidwell transformations
library(car)


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
# Generating New Variables
##################################################


# In Problem Set #6, we determined that taking logs
# of tractor prices produced a better model with
# a distribution closer to normal.

tractor_sales[, 'log_saleprice'] <- log(tractor_sales[, 'saleprice'])

# Create the variable squared_horsepower
# to investigate quadratic relationship of sale price to horsepower.
tractor_sales[, 'squared_horsepower'] <- tractor_sales[, 'horsepower']^2


##################################################
# Linear Regression Model
##################################################

# In a previous exercise I recommended the following model,
# which included a quadratic form for horsepower.

# Estimate a regression model.
lm_7 <- lm(data = tractor_sales,
           formula = log_saleprice ~
             horsepower + squared_horsepower +
             age +
             enghours +
             diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_7))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_sq_horse.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_7),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_sq_horse',
       caption = "Quadratic Model for Tractor Prices")



##################################################
# Box-Tidwell Transformation
##################################################

# The boxTidwell function tests for non-linear relationships
# to the mean of the dependent variable.
# The nonlinearity is in the form of an
# exponential transformation in the form of the Box-Cox
# transformation, except that the transformation is taken
# on the explanatory variables.

#--------------------------------------------------
# Transformation of horsepower
#--------------------------------------------------

# Modified from the linear model:
# log_saleprice ~ horsepower + squared_horsepower +
#   age + enghours +
#   diesel + fwd + manual + johndeere + cab
# This specification allows a single exponential
# transformation on horsepower, rather than a quadratic form.


bt_hp <- boxTidwell(formula =
                       log_saleprice ~ horsepower,
                    other.x = ~
                       # horsepower + squared_horsepower +
                       age +
                       enghours +
                       diesel + fwd + manual + johndeere + cab,
                    data = tractor_sales,
                    verbose = TRUE)

# The summary method is not available.
# summary(bt_hp)

# The output is a test on the exponent.
print(bt_hp)
# Note: The "MLE of lambda" is the exponent on horsepower.
# Similar to the Box-Cox transformation,
# with Box-Tidwell, the exponents are on the explanatory variables
# and are all called lambda.
# The exponent is significantly different from 0,
# although it is a small positive value,
# which suggests an increasing but sharply declining relationship.

# What does this transformation look like?
hp_grid <- seq(15, 500, by = 5)
bt_hp_lambda_hat <- 0.1143693
hp_bt_grid <- hp_grid^bt_hp_lambda_hat

plot(hp_grid,
     hp_bt_grid,
     xlab = 'Horsepower',
     ylab = 'Transformation of Horsepower',
     type = 'l',
     lwd = 3,
     col = 'blue')

# Print the output to a LaTeX file.
tab_file_name <- 'bt_hp.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_hp)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)


#--------------------------------------------------
# Transformation of age
#--------------------------------------------------


bt_age <- boxTidwell(formula =
                        log_saleprice ~ age,
                     other.x = ~
                        horsepower + squared_horsepower +
                        # age +
                        enghours +
                        diesel + fwd + manual + johndeere + cab,
                     data = tractor_sales,
                     verbose = TRUE)

print(bt_age)
# This coefficient is effectively 1, which is more evidence of
# a purely linear relationship between log_saleprice
# and age: the percentage depreciation rate is constant.

# Print the output to a LaTeX file.
tab_file_name <- 'bt_age.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_age)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)


#--------------------------------------------------
# Transformation of engine hours
#--------------------------------------------------


bt_eng <- boxTidwell(formula =
                        log_saleprice ~ enghours,
                     other.x = ~
                        horsepower + squared_horsepower +
                        age +
                        # enghours +
                        diesel + fwd + manual + johndeere + cab,
                     data = tractor_sales,
                     verbose = TRUE)

print(bt_eng)
# Although not statistically significant,
# this suggests a moderately decreasing relationship
# between log_saleprice and engine hours,
# which means that tractors with high hours of use
# depreciate more quickly with each additional hour of use.


# What does this transformation look like?
summary(tractor_sales[, 'enghours'])
eng_grid <- seq(1, 20001, by = 100)
bt_eng_lambda_hat <- 1.357793
eng_bt_grid <- eng_grid^bt_eng_lambda_hat


plot(eng_grid,
     eng_bt_grid,
     xlab = 'Engine Hours',
     ylab = 'Transformation of Engine Hours',
     type = 'l',
     lwd = 3,
     col = 'blue')

# Since a nonlinear relationship was detected with horsepower,
# check with nonlinearity in all three continuous variables.


bt_eng <- boxTidwell(formula =
                        log_saleprice ~ enghours,
                     other.x = ~
                        horsepower + squared_horsepower +
                        age +
                        # enghours +
                        diesel + fwd + manual + johndeere + cab,
                     data = tractor_sales)

print(bt_eng)


# Print the output to a LaTeX file.
tab_file_name <- 'bt_eng.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_eng)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)

#--------------------------------------------------
# Transformation of horsepower, ageand engine hours
#--------------------------------------------------

bt_full <- boxTidwell(formula =
                        log_saleprice ~ horsepower +
                         age +
                         enghours,
                     other.x = ~
                        # horsepower + squared_horsepower +
                        # age +
                        # enghours +
                        diesel + fwd + manual + johndeere + cab,
                     data = tractor_sales)

print(bt_full)

# This confirms the result of the above,
# with the only nonlinear transformation
# for horsepower.
# This suggests an additional model with
# this transformation of horsepower.

# Print the output to a LaTeX file.
tab_file_name <- 'bt_full.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_full)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)


##################################################
# Linear specification from the significant
# exponent in the Box-Tidwell transformation.
##################################################

print(bt_hp)
# MLE of lambda Score Statistic (z)     Pr(>|z|)
#     0.1143693           -7.386372 1.508894e-13
bt_hp_lambda_hat <- 0.1143693


# Create a variable horsepower_bt
# to investigate nonlinear relationship of log sale price to horsepower.
tractor_sales[, 'horsepower_bt'] <-
   tractor_sales[, 'horsepower']^bt_hp_lambda_hat

# Estimate a regression model.
lm_bt_hp <- lm(data = tractor_sales,
               formula = log_saleprice ~
                  horsepower_bt +
                  age +
                  enghours +
                  diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_bt_hp))

# The performance is similar to the other models with
# forms of nonlinearity for the value of horsepower.
# Put them in a table for a final comparison.

# Print the output to a LaTeX file.
tab_file_name <- 'reg_sq_horse_bt.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_7,
                lm_bt_hp),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_sq_horse_bt',
       caption = "Alternate Models for Tractor Prices")

# The last model has the highest R-squared
# among the ones we have estimated.
# The differences are marginal, however, so the practical recommendation
# is the model with the quadratic relationship for horsepower.


##################################################
# End
##################################################
