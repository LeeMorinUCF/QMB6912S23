##################################################
#
# QMB 6912 Capstone Project in Business Analytics
#
# Examples of Nonlinear Model Specification
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# March 22, 2022
#
##################################################
#
# Tractor_Nonparametric gives examples of linear
#   regression models augmented with a number of
#   different nonlinear model specifications,
#   which are estimated using nonparametric methods.
#
# Dependencies:
#   Libraries xtable and texreg
#   to print tables of regression results.
#   mgcv to fit the models within a generalized
#   additive model (GAM).
#
##################################################


##################################################
# Preparing the Workspace
##################################################


# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory, if running interactively.
# wd_path <- '~/GitHub/QMB6912S23/demo_08/Tractor_Nonparametric'
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

# Library mgcv for estimating Generalized Additive Models
library(mgcv)


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
# Linear Regression Model
##################################################

# In Problem Set #7 I recommended the following model,
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
# Linear Regression Model
# Frisch-Waugh-Lovell regressions to partial out
# other variables
##################################################


# Next, consider the model without this variable.

# Estimate a regression model.
lm_no_hp <- lm(data = tractor_sales,
                 formula = log_saleprice ~ # horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_no_hp))

# Next, estimate a model for the horsepower variable,
# using the other dependent variables as covariates.
# This estimates the "excess horsepower" above what one
# would predict using the other characteristics of the tractor.

# Estimate a regression model.
lm_hp <- lm(data = tractor_sales,
                     formula = horsepower ~
                       age + enghours +
                       diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_hp))

# Do the same for horsepower squared.
lm_hp_2 <- lm(data = tractor_sales,
            formula = squared_horsepower ~
              age + enghours +
              diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_hp_2))



# Finally, estimate a model for the
# value of a tractor using only the excess horsepower variable
# as a covariate.
# This regression is performed using the residuals
# from the two regressions using the other variables as covariates.

# Generate the residuals from each model.
tractor_sales[, 'horsepower_resid'] <- lm_hp$residuals
tractor_sales[, 'horsepower_2_resid'] <- lm_hp_2$residuals
tractor_sales[, 'log_saleprice_resid_hp'] <- lm_no_hp$residuals

# Finally, run a regression of the tractor price residuals
# on the horsepower residuals.
# This regression uses the Frisch-Waugh-Lovell theorem
# to partial out the other variables.


# Estimate a regression model.
lm_hp_quad_fwl <- lm(data = tractor_sales,
                  formula = log_saleprice_resid_hp ~ -1 +
                    horsepower_resid + horsepower_2_resid)

# Output the results to screen.
print(summary(lm_hp_quad_fwl))

# Notice that the coefficients on the horsepower variables
# are the same as those from the original regression.
print(summary(lm_7))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_sq_horse_fwl.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_7,
                lm_no_hp,
                lm_hp,
                lm_hp_2,
                lm_hp_quad_fwl),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_sq_horse_fwl',
       caption = "Quadratic Model for Tractor Prices: FWL Regressions")


##################################################
# Bivariate kernel estimation
##################################################

# You have used nonparametric methods to plot a density

# We can do something similar to predict one variable
# with the others.
# We will use the above transformations of the variables
# into residuals from regressions on the other variables.

#--------------------------------------------------
# Plot parametric model for horsepower
#--------------------------------------------------


# Plot a scattergraph to focus on horsepower.

fig_file_name <- 'dev_vs_horse.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(tractor_sales[, 'horsepower'],
     tractor_sales[, 'log_saleprice_resid_hp'],
     main = 'Quadratic Model for Tractor Prices',
     xlab = 'Horsepower',
     ylab = 'Deviation of Log Tractor Prices',
     col = 'blue')

# Add a line for the quadratic prediction from above.
points(tractor_sales[, 'horsepower'],
      predict(lm_hp_quad_fwl),
      lwd = 3, col = 'red')

dev.off()


# Plot a scattergraph to focus on excess horsepower.

fig_file_name <- 'dev_vs_horse_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(tractor_sales[, 'horsepower_resid'],
     tractor_sales[, 'log_saleprice_resid_hp'],
     main = 'Nonparametric Model for Tractor Prices',
     xlab = 'Deviation of Horsepower',
     ylab = 'Deviation of Log Tractor Prices',
     col = 'blue')

# Add a line for the quadratic prediction from above.
points(tractor_sales[, 'horsepower_resid'],
      predict(lm_hp_quad_fwl),
      lwd = 3, col = 'red')

dev.off()


#--------------------------------------------------
# Estimate and plot Nonparametric model for horsepower
#--------------------------------------------------

# The loess function is a smoothing method
# for estimating nonparametric models.
np_hp_fit_1 <- loess(log_saleprice_resid_hp ~ horsepower_resid,
                     tractor_sales)
# Calculate the predictions.
tractor_sales[, 'horsepower_np'] <- np_hp_fit_1$fitted


# Add a plot of this curve to the above scattergraph.


fig_file_name <- 'dev_np_vs_horse_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(tractor_sales[, 'horsepower_resid'],
     tractor_sales[, 'log_saleprice_resid_hp'],
     main = 'Nonparametric Model for Tractor Prices',
     xlab = 'Deviation of Horsepower',
     ylab = 'Deviation of Log Tractor Prices',
     col = 'blue')

# Add a line for the quadratic prediction from above.
points(tractor_sales[, 'horsepower_resid'],
       predict(lm_hp_quad_fwl),
       lwd = 3, col = 'red')

# Add a line for the quadratic prediction from above.
points(tractor_sales[, 'horsepower_resid'],
       np_hp_fit_1$fitted,
       lwd = 3, col = 'green')

dev.off()

# The nonparametric function is slightly more curved
# but the difference is not great.
# So far, it appears that the quadratic form
# is close enough.


#--------------------------------------------------
# Alternate models with different degrees of smoothing
#--------------------------------------------------

# When we estimated probability densities,
# we adjusted the bandwidth parameter to fit
# with different degrees of smoothness.
# The loess method has a span parameter for this function.
# The default smoother span (bandwidth parameter) is 0.75.

np_hp_fit_2 <- loess(log_saleprice_resid_hp ~ horsepower_resid,
                     tractor_sales, span = 2.0)

# Rebuild the previous plot to compare this estimate.
fig_file_name <- 'dev_np_vs_horse_dev_bw.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(tractor_sales[, 'horsepower_resid'],
     tractor_sales[, 'log_saleprice_resid_hp'],
     main = 'Nonparametric Model for Tractor Prices',
     xlab = 'Deviation of Horsepower',
     ylab = 'Deviation of Log Tractor Prices',
     col = 'blue')

# Add a line for the quadratic prediction from above.
points(tractor_sales[, 'horsepower_resid'],
       predict(lm_hp_quad_fwl),
       lwd = 3, col = 'red')

# Add a line for the quadratic prediction from above.
points(tractor_sales[, 'horsepower_resid'],
       np_hp_fit_1$fitted,
       lwd = 3, col = 'green')


# Add a plot of the smoother curve to the scattergraph.
points(tractor_sales[, 'horsepower_resid'],
       np_hp_fit_2$fitted,
       lwd = 2, col = 'orange')
# You can see some flattening with this
# more flexible estimator.


# Try again with less smoothing.
np_hp_fit_3 <- loess(log_saleprice_resid_hp ~ horsepower_resid,
                     tractor_sales, span = 0.1)


# Add a plot of this curve to the scattergraph.
points(tractor_sales[, 'horsepower_resid'],
       np_hp_fit_3$fitted,
       lwd = 2, col = 'magenta')
# Much more rough but you capture the decline
# in value for tractors with high horsepower.

dev.off()


# Ultimately, you would choose one that captures what
# is happening and don't need to show all of the curves
# that you fit during your investigation.

# In this case, we will keep the first fit.
tractor_sales[, 'horsepower_np'] <- np_hp_fit_1$fitted


# Try this again on other continuous variables.



#--------------------------------------------------
# Nonparametric model for age
#--------------------------------------------------


# First, fit Frish-Waugh-Lovell regressions
# to partial out other variables
# Consider the model without the age variable.

# Estimate a regression model.
lm_no_age <- lm(data = tractor_sales,
               formula = log_saleprice ~
                 horsepower + squared_horsepower +
                 # age +
                 enghours +
                 diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_no_age))

# Next, estimate a model for the age variable,
# using the other dependent variables as covariates.
# This estimates the "excess age" above what one
# would predict using the other characteristics of the tractor.

# Estimate a regression model.
lm_age <- lm(data = tractor_sales,
            formula = age ~
              horsepower + squared_horsepower +
              # age +
              enghours +
              diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_age))


# Finally, estimate a model for the
# value of a tractor using only the excess age variable
# as a covariate.
# This regression is performed using the residuals
# from the two regressions using the other variables as covariates.

# Generate the residuals from each model.
tractor_sales[, 'age_resid'] <- lm_age$residuals
tractor_sales[, 'log_saleprice_resid_age'] <- lm_no_age$residuals

# Finally, run a regression of the tractor price residuals
# on the age residuals.
# Again, this regression uses the Frisch-Waugh-Lovell theorem
# to partial out the other variables.


# Estimate a regression model.
lm_age_fwl <- lm(data = tractor_sales,
                     formula = log_saleprice_resid_age ~ -1 +
                       age_resid)

# Output the results to screen.
print(summary(lm_age_fwl))

# Notice that the coefficients on the age variable
# is the same as those from the original regression.
print(summary(lm_7))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_age_fwl.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_7,
                lm_no_age,
                lm_age,
                lm_age_fwl),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_age_fwl',
       caption = "Linear Model for Age: FWL Regressions")



# Plot a scattergraph to focus on age.

fig_file_name <- 'dev_vs_age.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(tractor_sales[, 'age'],
     tractor_sales[, 'log_saleprice_resid_age'],
     main = 'Quadratic Model for Tractor Prices',
     xlab = 'Age',
     ylab = 'Deviation of Log Tractor Prices',
     col = 'blue')

# Add a line for the linear prediction from above.
points(tractor_sales[, 'age'],
       predict(lm_age_fwl),
       lwd = 3, col = 'red')

dev.off()


# Plot a scattergraph to focus on excess age.

fig_file_name <- 'dev_vs_age_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(tractor_sales[, 'age_resid'],
     tractor_sales[, 'log_saleprice_resid_age'],
     main = 'Nonparametric Model for Tractor Prices',
     xlab = 'Deviation of Age',
     ylab = 'Deviation of Log Tractor Prices',
     col = 'blue')

# Add a line for the quadratic prediction from above.
points(tractor_sales[, 'age_resid'],
       predict(lm_age_fwl),
       lwd = 3, col = 'red')

dev.off()

# Notice that this is a straight line,
# since we have a single variable with no
# quadratic transformation.

#--------------------------------------------------
# Estimate and plot Nonparametric model for age
#--------------------------------------------------

# Use the loess function.
np_age_fit_1 <- loess(log_saleprice_resid_age ~ age_resid,
                     tractor_sales,
                     span = 0.25)
# Calculate the predictions.
tractor_sales[, 'age_np'] <- np_age_fit_1$fitted


# Add a plot of this curve to the above scattergraph.


fig_file_name <- 'dev_np_vs_age_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(tractor_sales[, 'age_resid'],
     tractor_sales[, 'log_saleprice_resid_age'],
     main = 'Nonparametric Model for Tractor Prices',
     xlab = 'Deviation of Age',
     ylab = 'Deviation of Log Tractor Prices',
     col = 'blue')

# Add a line for the linear prediction from above.
points(tractor_sales[, 'age_resid'],
       predict(lm_age_fwl),
       lwd = 3, col = 'red')

# Add a line for the nonparametric prediction.
points(tractor_sales[, 'age_resid'],
       np_age_fit_1$fitted,
       lwd = 3, col = 'green')

dev.off()


# Not much of a difference from the linear prediction.

# Try it with the remaining continuous variable.

#--------------------------------------------------
# Nonparametric model for engine hours
#--------------------------------------------------

# First, fit Frisch-Waugh-Lovell regressions
# to partial out other variables
# Consider the model without the engine hours variable.

# Estimate a regression model.
lm_no_eng <- lm(data = tractor_sales,
                formula = log_saleprice ~
                  horsepower + squared_horsepower +
                  age +
                  # enghours +
                  diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_no_eng))

# Next, estimate a model for the engine hours variable,
# using the other dependent variables as covariates.
# This estimates the "excess engine hours" above what one
# would predict using the other characteristics of the tractor.

# Estimate a regression model.
lm_eng <- lm(data = tractor_sales,
             formula = enghours ~
               horsepower + squared_horsepower +
               age +
               # enghours +
               diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_eng))


# Finally, estimate a model for the
# value of a tractor using only the excess engine hours variable
# as a covariate.
# This regression is performed using the residuals
# from the two regressions using the other variables as covariates.

# Generate the residuals from each model.
tractor_sales[, 'eng_resid'] <- lm_eng$residuals
tractor_sales[, 'log_saleprice_resid_eng'] <- lm_no_eng$residuals

# Finally, run a regression of the tractor price residuals
# on the engine hours residuals.
# Again, this regression uses the Frisch-Waugh-Lovell theorem
# to partial out the other variables.


# Estimate a regression model.
lm_eng_fwl <- lm(data = tractor_sales,
                 formula = log_saleprice_resid_eng ~ -1 +
                   eng_resid)

# Output the results to screen.
print(summary(lm_eng_fwl))

# Notice again that the coefficients on the engine hour variable
# is the same as those from the original regression.
print(summary(lm_7))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_eng_fwl.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_7,
                lm_no_eng,
                lm_eng,
                lm_eng_fwl),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_eng_fwl',
       caption = "Linear Model for Engine Hours: FWL Regressions")



# Plot a scattergraph to focus on eng.

fig_file_name <- 'dev_vs_eng.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(tractor_sales[, 'enghours'],
     tractor_sales[, 'log_saleprice_resid_eng'],
     main = 'Quadratic Model for Tractor Prices',
     xlab = 'Engine Hours',
     ylab = 'Deviation of Log Tractor Prices',
     col = 'blue')

# Add a line for the linear prediction from above.
points(tractor_sales[, 'enghours'],
       predict(lm_eng_fwl),
       lwd = 3, col = 'red')

dev.off()

# Skip the linear model and go straight to the
# nonparametric model.

#--------------------------------------------------
# Estimate and plot Nonparametric model for engine hours
#--------------------------------------------------

# Use the loess function.
np_eng_fit_1 <- loess(log_saleprice_resid_eng ~ eng_resid,
                      tractor_sales,
                      span = 0.25)
# Calculate the predictions.
tractor_sales[, 'eng_np'] <- np_eng_fit_1$fitted


# Add a plot of this curve to the above scattergraph.


fig_file_name <- 'dev_np_vs_eng_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(tractor_sales[, 'eng_resid'],
     tractor_sales[, 'log_saleprice_resid_eng'],
     main = 'Nonparametric Model for Tractor Prices',
     xlab = 'Deviation of Engine Hours',
     ylab = 'Deviation of Log Tractor Prices',
     col = 'blue')

# Add a line for the linear prediction from above.
points(tractor_sales[, 'eng_resid'],
       predict(lm_eng_fwl),
       lwd = 3, col = 'red')

# Add a line for the nonparametric prediction.
points(tractor_sales[, 'eng_resid'],
       np_eng_fit_1$fitted,
       lwd = 3, col = 'green')

dev.off()






# As with age, it looks as though linear might also be close enough.


##################################################
# Semiparametric Models
##################################################





#--------------------------------------------------
# Revisit our best parametric model
# Model 7: Linear model for log of dollar sale price
# With quadratic form for horsepower
#--------------------------------------------------

# Estimate a regression model.
lm_7 <- lm(data = tractor_sales,
                 formula = log_saleprice ~
                   horsepower + squared_horsepower +
                   age +
                   enghours +
                   diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_7))



#--------------------------------------------------
# Compare this with a semi-parametric model
# with a nonparametric fit on horsepower.
# Semiparametric model for log of dollar sale price
#--------------------------------------------------

# Estimate a regression model.
lm_sp_hp_1 <- lm(data = tractor_sales,
                 formula = log_saleprice ~
                   horsepower_np +
                   age +
                   enghours +
                   diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_sp_hp_1))

# The fit is slightly better but the model is very similar.


#--------------------------------------------------
# Compare this with a semi-parametric model
# with a nonparametric fit on age.
# Semiparametric model for log of dollar sale price
#--------------------------------------------------

# Estimate a regression model.
lm_sp_age_1 <- lm(data = tractor_sales,
                 formula = log_saleprice ~
                   horsepower + squared_horsepower +
                   age_np +
                   enghours +
                   diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_sp_age_1))

# Again, the fit is slightly better but the model is very similar.


#--------------------------------------------------
# Compare this with a semi-parametric model
# with a nonparametric fit on engine hours.
# Semiparametric model for log of dollar sale price
#--------------------------------------------------

# Estimate a regression model.
lm_sp_eng_1 <- lm(data = tractor_sales,
                  formula = log_saleprice ~
                    horsepower + squared_horsepower +
                    age +
                    eng_np +
                    diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_sp_eng_1))

# Again, the fit is slightly better but the model is very similar.

#--------------------------------------------------
# Compare this with a semi-parametric model
# with a nonparametric fit on engine hours.
# Semiparametric model for log of dollar sale price
#--------------------------------------------------

# Estimate a regression model.
lm_sp_full_1 <- lm(data = tractor_sales,
                  formula = log_saleprice ~
                    horsepower_np +
                    age_np +
                    eng_np +
                    diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_sp_full_1))

# Again, even with this aggressive step of including all three
# variables in a semiparametric form,
# the fit is still only slightly better and the model is very similar.


# Print the output to a LaTeX file.
tab_file_name <- 'reg_semipar.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_7,
                lm_sp_hp_1,
                lm_sp_age_1,
                lm_sp_eng_1,
                lm_sp_full_1),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_semipar',
       caption = "Semiparametric Models for Tractor Prices")








# The full model is still statistically better.
# The submodels are also both acceptable models.
# Of course, the more flexible model does better
# but this, in some sense, uses many more "degrees of freedom"
# so it is not a fair comparison.
# Better to estimate the semiparametric part in the Box-Tidwell
# transformation, which estimates these features jointly.
# We we will do this in a future problem set.

# With these results, I would explore the GAM or the Box-Tidwell
# with the horsepower variable a candidate for the nonparametric term.




##################################################
# Generalized Additive Model
##################################################

# Now consider a semiparametric model using an
# estimation method that accounts for the joint estimation
# of the nonparametric functions and the parameters.
# This form of model is termed a Generalized Additive Model (GAM)
# and can be estimated with the mgcv package.

# Begin with the linear model specification.
gam_model_lin <- gam(formula = log_saleprice ~
                       horsepower + squared_horsepower +
                       age +
                       enghours +
                       diesel + fwd + manual + johndeere + cab,
                     data = tractor_sales)
print(summary(gam_model_lin))

# Print the output to a LaTeX file.
# Since texreg does not work for GAMs,
# I just printed the output in verbatim mode.
tab_file_name <- 'reg_GAM_lin.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
summary(gam_model_lin)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)


# Allow for nonlinearity using the full model.
gam_model_full <- gam(formula = log_saleprice ~
                        s(horsepower) +
                        s(age) +
                        s(enghours) +
                        diesel + fwd + manual + johndeere + cab,
                      data = tractor_sales)

print(summary(gam_model_full))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_GAM_full.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
summary(gam_model_full)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)


# Allow for nonlinearity in the horsepower variable.
gam_model_hp <- gam(formula = log_saleprice ~
                      s(horsepower) +
                      age +
                      # s(age) +
                      enghours +
                      # s(enghours) +
                      diesel + fwd + manual + johndeere + cab,
                    data = tractor_sales)

print(summary(gam_model_hp))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_GAM_hp.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
summary(gam_model_hp)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)




# Print the output to a LaTeX file.
tab_file_name <- 'reg_GAM_full.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
summary(gam_model_full)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)



##################################################
# End
##################################################
