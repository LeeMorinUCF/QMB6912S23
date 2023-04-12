##################################################
#
# QMB 6912 Capstone Project in Business Analytics
#
# Examples of Model Specifications
# using Sample Selection Models
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# April 7, 2022
#
##################################################
#
# Tractor_SampleSelection gives examples of
#  sample selection models.
#
# Dependencies:
#   Libraries xtable and texreg
#   to print tables of regression results.
#   sampleSelection library to estimate models
#     with sample selection.
#
##################################################


##################################################
# Preparing the Workspace
##################################################


# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory, if running interactively.
# wd_path <- '~/GitHub/QMB6912S23/demo_10/Tractor_SampleSelection'
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


# library sampleSelection to estimate models
# with sample selection.
library(sampleSelection)


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

# Create a variable squared_horsepower
# to investigate quadratic relationship of sale price to horsepower.
tractor_sales[, 'squared_horsepower'] <- tractor_sales[, 'horsepower']^2


##################################################
# Benchmark Linear Regression Model
##################################################

# In Problem Set #7 I recommended the following model,
# which included a quadratic form for horsepower.

# Estimate a regression model.
lm_model_7 <- lm(data = tractor_sales,
                 formula = log_saleprice ~
                    horsepower + squared_horsepower +
                    age +
                    enghours +
                    diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_model_7))






# Print the output to a LaTeX file.
tab_file_name <- 'reg_sq_horse.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_7),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_sq_horse',
       caption = "Quadratic Model for Tractor Prices")


##################################################
# Separate Linear Regression Models
# by Brand Name: John Deere vs Others
##################################################

#--------------------------------------------------
# Estimating a Regression Model
# Models 12-13: Linear model for log of dollar sale price
# Separate Model for John Deere Tractors
#--------------------------------------------------


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


#--------------------------------------------------
# Estimating a Regression Model
# Models 14-15: Linear model for log of dollar sale price
# Separate Model for Tractors other than John Deere
#--------------------------------------------------


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
# Create Dependent Variables for
# Sample selection Models
##################################################

# The selection function requires each model
# (John Deere vs other brands) to be
# specified with a separate variable.

# Generate dependent variable in the outcome equation.
# Leave only what is observed.
tractor_sales[, 'log_price_other'] <-
   tractor_sales[, 'log_saleprice'] *
   (tractor_sales[, 'johndeere'] == 0)
tractor_sales[, 'log_price_JD'] <-
   tractor_sales[, 'log_saleprice'] *
   (tractor_sales[, 'johndeere'] == 1)



##################################################
# Summary of Variables by Brand
# to Investigate Sample Selection
##################################################


# As a preliminary step, compare the distributions of
# explanatory variables by brand of tractor.

# Compare continuous variables.
summary(tractor_sales[tractor_sales[, 'johndeere'] == 1,
                      c('horsepower', 'age', 'enghours')])
summary(tractor_sales[tractor_sales[, 'johndeere'] == 0,
                      c('horsepower', 'age', 'enghours')])
# Not much difference, so not much promise to indicate
# the brand name from these variables.

# Compare categorical variables.
summary(tractor_sales[tractor_sales[, 'johndeere'] == 1,
                      c('diesel', 'fwd', 'manual', 'cab')])
summary(tractor_sales[tractor_sales[, 'johndeere'] == 0,
                      c('diesel', 'fwd', 'manual', 'cab')])
# Greater differences in diesel and cab indicators,
# lesser differences between fwd and manual indicators.
# Use these in the probit model and selection model below.



##################################################
# Probit Models to Investigate Sample Selection
##################################################

# Now estimate a probit model to predict the selection indicator.
# Start with all the other variables in the model.
tobit_5_sel_probit1 <- glm(formula = johndeere ~
                              horsepower + squared_horsepower +
                              age +
                              enghours +
                              diesel + fwd + manual + cab,
                           data = tractor_sales,
                           family = binomial(link = "probit"))

summary(tobit_5_sel_probit1)
# John Deere tractors are more likely to be gasoline-powered,
# have manual transmissions, and less likely to have an enclosed cab.

# Estimate a reduced model.
# Eliminating variables one-by-one, I estimated the following model.
tobit_5_sel_probit2 <- glm(formula = johndeere ~
                              # horsepower + # squared_horsepower +
                              # age +
                              # enghours +
                              diesel +
                              # fwd +
                              manual + cab,
                           data = tractor_sales,
                           family = binomial(link = "probit"))

summary(tobit_5_sel_probit2)
# This should provide a concise but useful model to
# indicate the tractor designs that would be favored by John Deere
# engineers and customers.


# Print the output to a LaTeX file.
tab_file_name <- 'reg_probit.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(tobit_5_sel_probit1,
                tobit_5_sel_probit2),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_probit',
       caption = "Probit Models for Brand Selection of Tractors")


##################################################
# Sample selection Models
##################################################

#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate full model first
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.
# For a first model, use the entire set of variables
# for both observation equations, John Deere and others.

tobit_5_sel_1 <-
   selection(selection = johndeere ~
                diesel + manual + cab,
             outcome = list(log_price_other ~
                               horsepower +
                               squared_horsepower +
                               age +
                               enghours +
                               diesel +
                               fwd +
                               manual +
                               cab,
                            log_price_JD ~
                               horsepower +
                               squared_horsepower +
                               age +
                               enghours +
                               diesel +
                               fwd +
                               manual +
                               cab),
             iterlim = 20,
             # method = '2step',
             data = tractor_sales)
# Note that this printed some warning messages:
# Warning messages:
# 1: In heckit5fit(selection, as.formula(formula1), as.formula(formula2),  :
#  Inverse Mills Ratio is virtually multicollinear to the rest of explanatory
#  variables in the outcome equation 1
# 2: In heckit5fit(selection, as.formula(formula1), as.formula(formula2),  :
#  Inverse Mills Ratio is virtually multicollinear to the rest of explanatory
#  variables in the outcome equation 2

# This suggests much overlap between the two models.
# This is a risk of starting with too rich a model.

summary(tobit_5_sel_1)
# The coefficients seem fine but the standard errors are all infinite,
# possibly because a matrix in the standard errors is singular
# from the severe multicollinearity.

# Summary for linear model for first observation equation
# (brands other than John Deere).
summary(tobit_5_sel_1$lm1)

# Summary for linear model for second observation equation
# (John Deere tractors).
summary(tobit_5_sel_1$lm2)



# Instead of starting with a "big-to-small" approach,
# reconsider the best models for each tractor separately.

#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate reduced model from separate estimation
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.
# For a refined model, use the set of variables
# for separate observation equations,
# to match the reduced models with separate regressions
# for John Deere and others.

tobit_5_sel_2 <-
   selection(selection = johndeere ~
                diesel + manual + cab,
             outcome = list(log_price_other ~
                               horsepower +
                               squared_horsepower +
                               age +
                               enghours +
                               # diesel +
                               fwd +
                               manual +
                               cab,
                            log_price_JD ~
                               horsepower +
                               squared_horsepower +
                               age +
                               enghours +
                               # diesel +
                               # fwd +
                               # manual +
                               cab),
             iterlim = 20,
             method = '2step',
             data = tractor_sales)

summary(tobit_5_sel_2)

# Ideally, this produces standard errors in the above summary
# but we can look at the individual linear models
# for each regression equation.
# Note that these standard errors are underestimated,
# since they do not consider the variability from the estimation
# of the probit model for selection in the first stage.

# Summary for linear model for first observation equation
# (brands other than John Deere).
summary(tobit_5_sel_2$lm1)
# Suggests that we can drop the manual indicator.


# Summary for linear model for second observation equation
# (John Deere tractors).
summary(tobit_5_sel_2$lm2)
# The cab variable has marginal significance.


#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate reduced model without insignificant variables
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.

tobit_5_sel_3 <-
   selection(selection = johndeere ~
                diesel + manual + cab,
             outcome = list(log_price_other ~
                               horsepower +
                               squared_horsepower +
                               age +
                               enghours +
                               # diesel +
                               fwd +
                               # manual +
                               cab,
                            log_price_JD ~
                               horsepower +
                               squared_horsepower +
                               age # +
                               # enghours # +
                               # diesel +
                               # fwd +
                               # manual +
                               # cab
                            ),
             iterlim = 20,
             method = '2step',
             data = tractor_sales)

summary(tobit_5_sel_3)


# Summary for linear model for first observation equation
# (brands other than John Deere).
summary(tobit_5_sel_3$lm1)
# Suggests that we can drop the manual indicator.


# Summary for linear model for second observation equation
# (John Deere tractors).
summary(tobit_5_sel_3$lm2)




#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate small model and build up
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.

tobit_5_sel_4 <-
   selection(selection = johndeere ~
                # diesel +
                manual # +
                # cab
             ,
             outcome = list(log_price_other ~
                               horsepower +
                               # squared_horsepower +
                               age # +
                               # enghours +
                               # diesel +
                               # fwd +
                               # manual +
                               # cab
                               ,
                            log_price_JD ~
                               horsepower +
                               # squared_horsepower +
                               age # +
                            # enghours # +
                            # diesel +
                            # fwd +
                            # manual +
                            # cab
             ),
             iterlim = 20,
             method = '2step',
             data = tractor_sales)

summary(tobit_5_sel_4)


# Summary for linear model for first observation equation
# (brands other than John Deere).
summary(tobit_5_sel_4$lm1)


# Summary for linear model for second observation equation
# (John Deere tractors).
summary(tobit_5_sel_4$lm2)







# To print out results of the linear model,
# if included in the paper.

# # Print the output to a LaTeX file.
# tab_file_name <- 'selection_linear.tex'
# out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
# cat("\\begin{verbatim}", file = out_file_name)
# sink(out_file_name, append = TRUE)
# print(summary(tobit_5_sel_4$lm1))
# print(summary(tobit_5_sel_4$lm2))
# sink()
# cat("\\end{verbatim}", file = out_file_name, append = TRUE)


##################################################
# End
##################################################
