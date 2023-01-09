##################################################
#
# QMB 6912 Capstone Project in Business Analytics
#
# Examples of Sample Selection Models
# with the sampleSelection package.
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
# sampleSelection_examples.R gives examples of
#   regression models that adjust for sample selection.
#
# Dependencies:
#   sampleSelection library to estimate models
#     with sample selection.
#   mvtnorm library to generate multivariate
#     normal random variables.
#
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory, if running interactively.
# wd_path <- '~/GitHub/QMB6912S22/demo_10/sampleSelection_Examples'
# setwd(wd_path)


# # Set data directory.
# data_dir <- 'Data'
#
# # Set directory for storing figures.
# fig_dir <- 'Figures'
#
# # Set directory for storing tables.
# tab_dir <- 'Tables'


##################################################
# Load libraries
##################################################

# library sampleSelection to estimate models
# with sample selection.
library(sampleSelection)

# library mvtnorm to generate multivariate normal
# random variables.
library(mvtnorm)


##################################################
# Example 1: Type-2 Tobit Model
##################################################

# First, we estimate a correctly specified
# Tobit-2 model with exclusion restriction.

#--------------------------------------------------
# Generate data
#--------------------------------------------------

# Setting the seed fixes the sequence of random numbers.
set.seed(0)

# Generate matrix of random error terms.
tobit_2_ex1 <- data.frame(rmvnorm(500, c(0, 0),
               matrix(c( 1.0, -0.7,
                         -0.7,  1.0), 2, 2)))
colnames(tobit_2_ex1) <- c('eps1', 'eps2')

# Generate selection variable (unobserved).
tobit_2_ex1[, 'xs'] <- runif(500)

# Generate logical selection variable in the selection equation.
tobit_2_ex1[, 'ys'] <- tobit_2_ex1[, 'xs'] +
  tobit_2_ex1[, 'eps1'] > 0
# Note that true slope coefficient is one
# and the true intercept coefficient is zero.


# Generate explanatory variable in the outcome equation (observed).
tobit_2_ex1[, 'xo'] <- runif(500)

# Generate dependent variable (partially observed).
tobit_2_ex1[, 'yoX'] <- tobit_2_ex1[, 'xo'] +
  tobit_2_ex1[, 'eps2']
# Note again that true slope coefficient is one
# and the true intercept coefficient is zero.

# Generate dependent variable in the outcome equation (observed).
tobit_2_ex1[, 'yo'] <- tobit_2_ex1[, 'yoX'] *
  (tobit_2_ex1[, 'ys'] > 0)



# Note the observations set to zero.
summary(tobit_2_ex1)

# Inspect only the observed events.
summary(tobit_2_ex1[tobit_2_ex1[, 'ys'], ])



#--------------------------------------------------
# Investigate Sample Selection
#--------------------------------------------------

# Since the data are generated above, we know that the
# selection is based on the variable xs.
# With real data, you wouldn't know that and would have
# to decide which variables to put in the selection
# equation of the model.
# A probit or logistic model can be used to predict the
# selection indicator.

# Check the potential of either xs or xo.
# The other variables would not be observed in real life.
tobit_2_sel_probit2 <- glm(formula = ys ~ xs + xo,
                        data = tobit_2_ex1,
                        family = binomial(link = "probit"))

summary(tobit_2_sel_probit2)

# Looks like the variable xs is the only one to use.
# Try again with only that one variable.
tobit_2_sel_probit2 <- glm(formula = ys ~ xs,
                           data = tobit_2_ex1,
                           family = binomial(link = "probit"))

summary(tobit_2_sel_probit2)
# Compare these numbers to the estimates from the next model.

#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
#--------------------------------------------------

# This takes two equations. The first is
# a specification of the equation that determines
# when an observation is selected.
# It has a binary dependent variable.
# The second is the regression equation,
# using an independent explanatory variable.
tobit_2_sel_1 <- selection(selection = ys ~ xs,
                           outcome = yo ~ xo,
                           data = tobit_2_ex1)

summary(tobit_2_sel_1)
# Notice the slope coefficients are close to 1 (the true values).

# Overall, the true values are within the
# 95% confidence intervals of the corresponding estimates.




#--------------------------------------------------
# Estimate Model that Ignores Sample Selection
#--------------------------------------------------



# Estimate the feasible linear model
# with some rows observed to be zero.
tobit_2_lm_1_full <- lm(formula = yo ~ xo,
                   data = tobit_2_ex1)

summary(tobit_2_lm_1_full)
# Notice the coefficients differ from the true values.
# The slope is biased toward zero.


# Although it is infeasible, as not all data are observed
# If we had observed the full set of observations,
# the linear model would be correctly specified.

tobit_2_lm_1_inf <- lm(formula = yoX ~ xo,
                       data = tobit_2_ex1)

summary(tobit_2_lm_1_inf)

# Although infeasible, the intercept and slope coefficients
# are close to zero and one (the true values).



# We can, however, estimate the model
# with only the variables observed.
# It drops all the information about the observations
# that were not selected.
tobit_2_lm_1_sel <- lm(formula = yoX ~ xo,
                       data = tobit_2_ex1[tobit_2_ex1[, 'ys'], ])

summary(tobit_2_lm_1_sel)

# In this case, the intercept and slope coefficients
# are close to zero and one (the true values).


# Since this drops observations with partial information,
# however, these estimates are more variable.



#--------------------------------------------------
# Compare Models With and Without Sample Selection
#--------------------------------------------------


# Now compare these estimates graphically.

# fig_file_name <- 'tobit_2_sel_1.pdf'
# out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# pdf(out_file_name)

plot(tobit_2_ex1[, 'xo'],
     tobit_2_ex1[, 'yoX'],
     main = c('Observations Selected (red) and Unobserved (blue)',
              '(Independent Selection and Explanatory Variables)'),
     xlab = 'Explanatory Variable (xo)',
     ylab = 'Dependent Variable (Yox)',
     col = 'blue')

# Color the observations that pass the selection.
points(tobit_2_ex1[tobit_2_ex1[, 'ys'], 'xo'],
       tobit_2_ex1[tobit_2_ex1[, 'ys'], 'yo'],
       # lwd = 3,
       col = 'red')

# Add a line for the linear relationship from the true model.
xo_grid <- seq(0, 1, by = 0.01)
lines(xo_grid,
      xo_grid,
      lwd = 3,
      col = 'black')


# Add a line for the linear prediction from the selection model.
lines(xo_grid,
      predict(tobit_2_sel_1,
              newdata = data.frame(xo = xo_grid)),
      lwd = 3,
      col = 'black',
      lty = 2)

# Add a line for the linear prediction from the feasible regression model.
lines(xo_grid,
      predict(tobit_2_lm_1_sel,
              newdata = data.frame(xo = xo_grid)),
      lwd = 3,
      col = 'black',
      lty = 3)

# dev.off()


# The prediction from the sample selection model
# (the dashed line) is closer to the true relationship
# (the solid line) than is the prediction from the
# linear model on the selected data only (dotted line).

# The prediction from the regression model is downward biased
# because it does not take into account the fact that we tend
# to observe the observations with low realizations of eps1.

# As expected, the slope is steeper for the linear model.


##################################################
# Example 2: Type-2 Tobit Model
##################################################

# Next, we estimate a correctly specified
# Tobit-2 model without the exclusion restriction.
# This time we use the same explanatory
# variable as the selection variable (xs),
# instead of an independent variable (xo)
# in the equation for yoX.



#--------------------------------------------------
# Generate data
#--------------------------------------------------

# Copy variables from the previous example
# for greater comparability.
tobit_2_ex2 <- tobit_2_ex1[, c('eps1', 'eps2', 'xs', 'ys')]

# Generate dependent variable (partially observed).
tobit_2_ex2[, 'yoX'] <- tobit_2_ex2[, 'xs'] +
  tobit_2_ex2[, 'eps2']
# Note again that true slope coefficient is one
# and the true intercept coefficient is zero.

# Generate dependent variable in the outcome equation (observed).
tobit_2_ex2[, 'yo'] <- tobit_2_ex2[, 'yoX'] *
  (tobit_2_ex2[, 'ys'] > 0)



#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
#--------------------------------------------------

# This time, both equations have the same explanatory
# variable, so the independence assumption is not satisfied.
tobit_2_sel_2 <- selection(selection = ys ~ xs,
                           outcome = yo ~ xs,
                           data = tobit_2_ex2)

summary(tobit_2_sel_2)
# The estimates are still unbiased but standard errors
# are substantially larger in this case.
# The exclusion restriction (independent information about
# the selection process) has a certain identifying power that
# we now have lost.

# The result:
# If you have access to a separate variable that
# predicts when the data will be observed, then
# the estimates will be estimated more accurately.


#--------------------------------------------------
# Estimate Model that Ignores Sample Selection
#--------------------------------------------------

# Estimate the feasible linear regression model
# with only the variables observed.
# It drops all the information about the observations
# that were not selected.
tobit_2_lm_2_sel <- lm(formula = yoX ~ xs,
                       data = tobit_2_ex2[tobit_2_ex2[, 'ys'], ])

summary(tobit_2_lm_2_sel)

#--------------------------------------------------
# Compare Models With and Without Sample Selection
#--------------------------------------------------

# Now compare these estimates graphically.

# fig_file_name <- 'tobit_2_sel_2.pdf'
# out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# pdf(out_file_name)

plot(tobit_2_ex2[, 'xs'],
     tobit_2_ex2[, 'yoX'],
     main = c('Observations Selected (red) and Unobserved (blue)',
              '(Perfectly Correlated Selection and Explanatory Variables)'),
     xlab = 'Explanatory Variable (xs)',
     ylab = 'Dependent Variable (Yox)',
     col = 'blue')

# Color the observations that pass the selection.
points(tobit_2_ex2[tobit_2_ex2[, 'ys'], 'xs'],
       tobit_2_ex2[tobit_2_ex2[, 'ys'], 'yo'],
       # lwd = 3,
       col = 'red')

# Add a line for the linear relationship from the true model.
# xo_grid <- seq(0, 1, by = 0.01)
lines(xo_grid,
      xo_grid,
      lwd = 3,
      col = 'black')


# Add a line for the linear prediction from the selection model.
lines(xo_grid,
      predict(tobit_2_sel_2,
              newdata = data.frame(xs = xo_grid)),
      lwd = 3,
      col = 'black',
      lty = 2)

# Add a line for the linear prediction from the feasible regression model.
lines(xo_grid,
      predict(tobit_2_lm_2_sel,
              newdata = data.frame(xs = xo_grid)),
      lwd = 3,
      col = 'black',
      lty = 3)

# dev.off()

# The selection model (dashed line) is slightly closer
# to the true relationship (solid line) but offers only a
# slight improvement over the linear model (dotted line),
# when the selection is not independent of the
# explanatory variable.
# The slope is still slightly steeper for the linear model.


##################################################
# Example 3: Type-2 Tobit Model
##################################################

# Next, we estimate a correctly specified
# Tobit-2 model with exclusion restriction.
# This time we provide more variation
# in the latent selection equation.



#--------------------------------------------------
# Generate data
#--------------------------------------------------

# Copy variables from the first example
# for greater comparability.
tobit_2_ex3 <- tobit_2_ex1[, c('eps1', 'eps2')]

# Generate selection variable (unobserved).
tobit_2_ex3[, 'xs'] <- runif(500, -5, 5)

# Generate logical selection variable in the selection equation.
tobit_2_ex3[, 'ys'] <- tobit_2_ex3[, 'xs'] +
  tobit_2_ex3[, 'eps1'] > 0
# Note that true slope coefficient is one
# and the true intercept coefficient is zero.


# Generate dependent variable (partially observed).
tobit_2_ex3[, 'yoX'] <- tobit_2_ex3[, 'xs'] +
  tobit_2_ex3[, 'eps2']
# Note again that true slope coefficient is one
# and the true intercept coefficient is zero.

# Generate dependent variable in the outcome equation (observed).
tobit_2_ex3[, 'yo'] <- tobit_2_ex3[, 'yoX'] *
  (tobit_2_ex3[, 'ys'] > 0)

#--------------------------------------------------
# Investigate Sample Selection
#--------------------------------------------------

# Again, since the data are generated above, we know that the
# selection is based on the variable xs.
# With real data, you wouldn't know that and would have
# to decide which variables to put in the selection
# equation of the model.
# Again, we use the probit model to predict the
# selection indicator.

# In this case, the variable xs is the only one to use.
# The other variables would not be observed in real life.
tobit_2_sel_probit3 <- glm(formula = ys ~ xs,
                           data = tobit_2_ex3,
                           family = binomial(link = "probit"))

summary(tobit_2_sel_probit3)

# Compare these numbers to the estimates from the next model.


#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
#--------------------------------------------------

# Again, both equations have the same explanatory
# variable, so the independence assumption is not satisfied.
tobit_2_sel_3 <- selection(selection = ys ~ xs,
                           outcome = yo ~ xs,
                           data = tobit_2_ex3)

summary(tobit_2_sel_3)
# This time it gives very accurate readings of the slope coefficients
# because when the selection variable provides more
# information about the probability of selection,
# the selection bias is more accurately accounted for.

#--------------------------------------------------
# Estimate Model that Ignores Sample Selection
#--------------------------------------------------

# Estimate the feasible linear regression model
# with only the variables observed.
# It drops all the information about the observations
# that were not selected.
tobit_2_lm_3_sel <- lm(formula = yoX ~ xs,
                       data = tobit_2_ex3[tobit_2_ex3[, 'ys'], ])

summary(tobit_2_lm_3_sel)
# The variance is also smaller with more variability in the
# selection variable.


#--------------------------------------------------
# Compare Models With and Without Sample Selection
#--------------------------------------------------

# Now compare these estimates graphically.

# fig_file_name <- 'tobit_2_sel_2.pdf'
# out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# pdf(out_file_name)

plot(tobit_2_ex3[, 'xs'],
     tobit_2_ex3[, 'yoX'],
     main = c('Observations Selected (red) and Unobserved (blue)',
              '(Perfectly Correlated Selection and Explanatory Variables)'),
     xlab = 'Explanatory Variable (xs)',
     ylab = 'Dependent Variable (Yox)',
     col = 'blue')

# Color the observations that pass the selection.
points(tobit_2_ex3[tobit_2_ex3[, 'ys'], 'xs'],
       tobit_2_ex3[tobit_2_ex3[, 'ys'], 'yo'],
       # lwd = 3,
       col = 'red')

# Add a line for the linear relationship from the true model.
xs_grid <- seq(-5, 5, by = 0.01)
lines(xs_grid,
      xs_grid,
      lwd = 3,
      col = 'black')


# Add a line for the linear prediction from the selection model.
lines(xs_grid,
      predict(tobit_2_sel_3,
              newdata = data.frame(xs = xs_grid)),
      lwd = 3,
      col = 'black',
      lty = 2)

# Add a line for the linear prediction from the feasible regression model.
lines(xs_grid,
      predict(tobit_2_lm_3_sel,
              newdata = data.frame(xs = xs_grid)),
      lwd = 3,
      col = 'black',
      lty = 3)

# dev.off()

# Again, the selection model (dashed line) is closer
# to the true relationship (solid line) than the
# linear model (dotted line), when the selection is
# not independent of the explanatory variable
# but has enough variance to provide a clean sample.
# Again, the slope is steeper for the linear model.


##################################################
# Example 4: Type-5 Tobit Model
##################################################

# Next, we estimate a correctly specified
# Tobit-5 model with a switching regression model.
# That is, instead of being observed or unobserved,
# the outcome is recorded from the result of one or two models.
# In this version, the selection variable is independent
# of the explanatory variable.




#--------------------------------------------------
# Generate data
#--------------------------------------------------

set.seed(0)

# Create a trivariate covariance matrix.
vc <- diag(3)
vc[lower.tri(vc)] <- c(0.9, 0.5, 0.1)
vc[upper.tri(vc)] <- vc[lower.tri(vc)]


# Generate matrix of random error terms.
tobit_5_ex4 <- data.frame(rmvnorm(500, c(0, 0, 0), vc))
colnames(tobit_5_ex4) <- c('eps1', 'eps2', 'eps3')

# Generate selection variable (unobserved).
tobit_5_ex4[, 'xs'] <- runif(500)

# Generate logical selection variable in the selection equation.
tobit_5_ex4[, 'ys'] <- tobit_5_ex4[, 'xs'] +
  tobit_5_ex4[, 'eps1'] > 0
# Note that true slope coefficient is one
# and the true intercept coefficient is zero.


# Generate explanatory variables in the outcome equation (observed).
# Model 1:
tobit_5_ex4[, 'xo1'] <- runif(500)
# Model 2:
tobit_5_ex4[, 'xo2'] <- runif(500)


# Generate dependent variable (partially observed).
# Model 1:
tobit_5_ex4[, 'yo1'] <- tobit_5_ex4[, 'xo1'] +
  tobit_5_ex4[, 'eps2']
# Model 2:
tobit_5_ex4[, 'yo2'] <- tobit_5_ex4[, 'xo2'] +
  tobit_5_ex4[, 'eps3']
# Note again that true slope coefficients are one
# and the true intercept coefficients are zero.



# Generate dependent variable in the outcome equation.
# (Leave only what is observed.)
tobit_5_ex4[, 'yo1'] <- tobit_5_ex4[, 'yo1'] *
  (tobit_5_ex4[, 'ys'] <= 0)
tobit_5_ex4[, 'yo2'] <- tobit_5_ex4[, 'yo2'] *
  (tobit_5_ex4[, 'ys'] > 0)
# Not required, since the likelihood function
# uses only the observations for the corresponding
# model based on whether ys is TRUE or FALSE.


#--------------------------------------------------
# Investigate Sample Selection
#--------------------------------------------------

# Same as with the above Tobit-2 models,
# since the data are generated above, we know that the
# selection is based on the variable xs only.
# Again, with real data, you wouldn't know that and would have
# to decide which variables to put in the selection
# equation of the model.
# I estimate a probit model to predict the selection indicator.

# Check the potential of xs, xo1 or xo2.
# The other variables would not be observed in real life.
tobit_5_sel_probit4 <- glm(formula = ys ~ xs + xo1 + xo2,
                           data = tobit_5_ex4,
                           family = binomial(link = "probit"))

summary(tobit_5_sel_probit4)

# Looks like the variable xs is the only one to use.
# Try again with only that one variable.
tobit_5_sel_probit4 <- glm(formula = ys ~ xs,
                           data = tobit_5_ex4,
                           family = binomial(link = "probit"))

summary(tobit_5_sel_probit4)
# Compare these numbers to the estimates from the next model.


#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
#--------------------------------------------------

# This time, as in example 1, both equations
# have different explanatory variables,
# which are different from the selection variable,
# so the independence assumption is satisfied.
tobit_5_sel_4 <- selection(selection = ys ~ xs,
                           outcome = list(yo1 ~ xo1,
                                          yo2 ~ xo2),
                           iterlim = 20,
                           data = tobit_5_ex4)

summary(tobit_5_sel_4)



#--------------------------------------------------
# Estimate Model that Ignores Sample Selection
#--------------------------------------------------

# Estimate the feasible linear regression model
# with only the variables observed.

# It drops all the information about the observations
# that were not selected for the particular model.

# Model 1:
tobit_5_lm_4_sel1 <- lm(formula = yo1 ~ xo1,
                        data = tobit_5_ex4[!tobit_5_ex4[, 'ys'], ])

summary(tobit_5_lm_4_sel1)

# Model 2:
tobit_5_lm_4_sel2 <- lm(formula = yo2 ~ xo2,
                        data = tobit_5_ex4[tobit_5_ex4[, 'ys'], ])

summary(tobit_5_lm_4_sel2)

# The results are similar but the variances are larger
# as each regression model ignores the information
# from the other equation.


##################################################
# Example 5: Type-5 Tobit Model
##################################################

# Next, we estimate a correctly specified
# Tobit-5 model with a switching regression model.
# That is, instead of being observed or unobserved,
# the outcome is recorded from the result of one or two models.
# In this version, the selection variable is perfectly
# correlated with the explanatory variable.



#--------------------------------------------------
# Generate data
#--------------------------------------------------

set.seed(6)

# Copy same matrix of random error terms.
# tobit_5_ex5 <- tobit_5_ex4[, c('eps1', 'eps2', 'eps3')]
# Instead, generate new matrix with twice as many observations.

# Generate matrix of random error terms.
tobit_5_ex5 <- data.frame(rmvnorm(1000, c(0, 0, 0), vc))
colnames(tobit_5_ex5) <- c('eps1', 'eps2', 'eps3')
# Also, this uses a multivariate normal distribution of errors,
# which is the true specification of the Tobit model.
# The examples in the paper by Toomet and Henningsen
# use a misspecified model with errors following the Chi-squared
# distribution.
# In those examples, large biases occur, which suggests that
# the estimates are sensitive to the distribution of errors.
# Although you do not observe the errors in the selection equation,
# you can at least choose your model for each of the equations
# such that the distributions of residuals are normal.

# Generate selection variable (unobserved).
tobit_5_ex5[, 'xs'] <- runif(1000, -1, 1)


# Generate logical selection variable in the selection equation.
tobit_5_ex5[, 'ys'] <- tobit_5_ex5[, 'xs'] +
  tobit_5_ex5[, 'eps1'] > 0
# Note that true slope coefficient is one
# and the true intercept coefficient is zero.


# Generate dependent variable (partially observed).
# Model 1:
tobit_5_ex5[, 'yo1'] <- tobit_5_ex5[, 'xs'] +
  tobit_5_ex5[, 'eps2']
# Model 2:
tobit_5_ex5[, 'yo2'] <- tobit_5_ex5[, 'xs'] +
  tobit_5_ex5[, 'eps3']
# Note again that true slope coefficients are one
# and the true intercept coefficients are zero.



# Generate dependent variable in the outcome equation.
# (Leave only what is observed.)
tobit_5_ex5[, 'yo1'] <- tobit_5_ex5[, 'yo1'] *
  (tobit_5_ex5[, 'ys'] <= 0)
tobit_5_ex5[, 'yo2'] <- tobit_5_ex5[, 'yo2'] *
  (tobit_5_ex5[, 'ys'] > 0)
# Not required, since the likelihood function
# uses only the observations for the corresponding
# model based on whether ys is TRUE or FALSE.


#--------------------------------------------------
# Investigate Sample Selection
#--------------------------------------------------

# Same as with the above Tobit-2 models,
# I estimate a probit model to predict the selection indicator.
# In this case, the variable xs is the only one to use.
tobit_5_sel_probit5 <- glm(formula = ys ~ xs,
                           data = tobit_5_ex5,
                           family = binomial(link = "probit"))

summary(tobit_5_sel_probit5)
# Compare these numbers to the estimates from the next model.


#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
#--------------------------------------------------

# As in examples 2 and 3, both equations have the same explanatory
# variable, so the independence assumption is not satisfied.
tobit_5_sel_5 <- selection(selection = ys ~ xs,
                           outcome = list(yo1 ~ xs,
                                          yo2 ~ xs),
                           iterlim = 20,
                           data = tobit_5_ex5)

summary(tobit_5_sel_5)



#--------------------------------------------------
# Estimate Model that Ignores Sample Selection
#--------------------------------------------------


# Within the Tobit-5 model, there exist two models.
# Estimate the feasible linear regression model
# with only the variables observed.

# It drops all the information about the observations
# that were not selected for the particular model.

# Model 1:
tobit_5_lm_5_sel1 <- lm(formula = yo1 ~ xs,
                        data = tobit_5_ex5[!tobit_5_ex5[, 'ys'], ])

summary(tobit_5_lm_5_sel1)

# Model 2:
tobit_5_lm_5_sel2 <- lm(formula = yo2 ~ xs,
                        data = tobit_5_ex5[tobit_5_ex5[, 'ys'], ])

summary(tobit_5_lm_5_sel2)


# The results are similar but the variances are larger
# as each regression model ignores the information
# from the other equation.


# Overall, specification error in the error distribution
# can cause serious problems, so it is important
# to specify the models so that the residuals are normal.
# Conversely, the independence assumption is not critical
# to the accuracy of the estimates. Independent selection
# variables will improve accuracy but the model still works
# even if the selection variable is perfectly correlated
# with the variables in each of the switching models.


##################################################
# End
##################################################

