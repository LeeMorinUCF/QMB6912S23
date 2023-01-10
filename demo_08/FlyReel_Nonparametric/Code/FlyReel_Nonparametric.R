##################################################
#
# QMB 6912 Capstone Project in Business Analytics
#
# Examples of Nonlinear Model Specfication
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
# FlyReel_Nonparametric gives examples of linear
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
# wd_path <- '~/GitHub/QMB6912S23/demo_08/FlyReel_Nonparametric'
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
# Load Data
##################################################

# Set parameters for flyreel dataset.
in_file_name <- sprintf('%s/%s', data_dir, 'FlyReels.csv')
fly_col_names <- c('Name', 'Brand', 'Weight', 'Diameter', 'Width',
                   'Price', 'Sealed', 'Country', 'Machined')

# Load data.
flyreels <- read.csv(file = in_file_name, header = FALSE,
                     col.names = fly_col_names)

# Initial inspection.
print('Summary of FlyReels Dataset:')
print(summary(flyreels))



##################################################
# Generating Variables
##################################################

# Set categorical variables as factors.
cat_var_list <- colnames(flyreels)[lapply(flyreels, class) == "character"]
for (var_name in cat_var_list) {
  flyreels[, var_name] <- as.factor(flyreels[, var_name])
}

# Initial inspection.
print('FlyReels Dataset with Categorical Factors:')
print(summary(flyreels))



# Create a density variable.
colnames(flyreels)
flyreels[, 'Volume'] <- pi * (flyreels[, 'Diameter']/2)^2 * flyreels[, 'Width']
flyreels[, 'Density'] <- flyreels[, 'Weight'] / flyreels[, 'Volume']

# Create logarithm of dependent variable.
flyreels[, 'log_Price'] <- log(flyreels[, 'Price'])


# Replace Country Indicator with made_in_USA Indicator.
table(flyreels[, 'Country'], useNA = 'ifany')
flyreels[, 'made_in_USA'] <- flyreels[, 'Country'] == 'USA'
# Check:
table(flyreels[, 'Country'],
      flyreels[, 'made_in_USA'], useNA = 'ifany')


##################################################
# Linear Regression Model
##################################################

# In Problem Set #7 I recommended the following
# linear regression model.

#--------------------------------------------------
# Interaction with Sealed and made_in_USA
#--------------------------------------------------

# As determined in Problem Sets 6 and 7,
# set target variable as the log transformation
# for remaining analysis.
target_var <- 'log_Price'

# Specify list of variables with interactions.
var_list <- c('Width', 'Diameter', 'Density',
              'Sealed', 'Machined', 'made_in_USA',
              'made_in_USA*Sealed')
# Notice that we keep the made_in_USA indicator
# to maintain different intercept by country.


# Specify the regression formula.
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

# Estimate the model and output to screen.
lm_6 <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_6))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_sealed_USA.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_6),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_sealed_USA',
       caption = "Linear Model for Fly Reel Prices")


##################################################
# Linear Regression Model
# Frisch-Waugh-Lovell regressions to partial out
# other variables
##################################################

# Since we know we will investigate several variables,
# let's perform all of the FLW regressions
# to obtain the residuals we need.

#--------------------------------------------------
# FWL regressions for width.
#--------------------------------------------------

fwl_var <- 'Width'
fwl_var_list <- var_list[var_list != fwl_var]

# Regression for the dependent variable.
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(fwl_var_list, collapse = ' + ')))

lm_no_width <- lm(data = flyreels, formula = lm_fmla)
flyreels[, 'log_Price_resid_width'] <- lm_no_width$residuals


# Regression for the explanatory variable.
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              fwl_var,
                              paste(fwl_var_list, collapse = ' + ')))

lm_width <- lm(data = flyreels, formula = lm_fmla)
flyreels[, 'width_resid'] <- lm_width$residuals

#--------------------------------------------------
# FWL regressions for Diameter.
#--------------------------------------------------

fwl_var <- 'Diameter'
fwl_var_list <- var_list[var_list != fwl_var]

# Regression for the dependent variable.
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(fwl_var_list, collapse = ' + ')))

lm_no_diameter <- lm(data = flyreels, formula = lm_fmla)
flyreels[, 'log_Price_resid_diameter'] <- lm_no_diameter$residuals


# Regression for the explanatory variable.
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              fwl_var,
                              paste(fwl_var_list, collapse = ' + ')))

lm_diameter <- lm(data = flyreels, formula = lm_fmla)
flyreels[, 'diameter_resid'] <- lm_diameter$residuals

#--------------------------------------------------
# FWL regressions for Density.
#--------------------------------------------------

fwl_var <- 'Density'
fwl_var_list <- var_list[var_list != fwl_var]

# Regression for the dependent variable.
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(fwl_var_list, collapse = ' + ')))

lm_no_density <- lm(data = flyreels, formula = lm_fmla)
flyreels[, 'log_Price_resid_density'] <- lm_no_density$residuals


# Regression for the explanatory variable.
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              fwl_var,
                              paste(fwl_var_list, collapse = ' + ')))

lm_density <- lm(data = flyreels, formula = lm_fmla)
flyreels[, 'density_resid'] <- lm_density$residuals



##################################################
# Verify FWL Transformations for Regression Models
##################################################

# This section is unnecessary but I include it to check that
# the FWL regressions were performed correctly.

#--------------------------------------------------
# FWL regression model for Width
#--------------------------------------------------

fwl_var <- 'width'
fwl_target_var <- sprintf('log_Price_resid_%s', fwl_var)
fwl_exp_var <- sprintf('%s_resid', fwl_var)

# Formula excludes the intercept (specified by the -1).
lm_fmla <- as.formula(sprintf('%s ~ -1 + %s',
                              fwl_target_var,
                              fwl_exp_var))

# Estimate a regression model.
lm_width_fwl <- lm(data = flyreels, formula = lm_fmla)


# Output the results to screen.
print(summary(lm_width_fwl))



#--------------------------------------------------
# FWL regression model for Diameter
#--------------------------------------------------

fwl_var <- 'diameter'
fwl_target_var <- sprintf('log_Price_resid_%s', fwl_var)
fwl_exp_var <- sprintf('%s_resid', fwl_var)

# Formula excludes the intercept (specified by the -1).
lm_fmla <- as.formula(sprintf('%s ~ -1 + %s',
                              fwl_target_var,
                              fwl_exp_var))

# Estimate a regression model.
lm_diameter_fwl <- lm(data = flyreels, formula = lm_fmla)


# Output the results to screen.
print(summary(lm_diameter_fwl))


#--------------------------------------------------
# FWL regression model for Density
#--------------------------------------------------

fwl_var <- 'density'
fwl_target_var <- sprintf('log_Price_resid_%s', fwl_var)
fwl_exp_var <- sprintf('%s_resid', fwl_var)

# Formula excludes the intercept (specified by the -1).
lm_fmla <- as.formula(sprintf('%s ~ -1 + %s',
                              fwl_target_var,
                              fwl_exp_var))

# Estimate a regression model.
lm_density_fwl <- lm(data = flyreels, formula = lm_fmla)


# Output the results to screen.
print(summary(lm_density_fwl))



# Note that the coefficients on the variables
# are the same as those from the original regression.
print(summary(lm_6))






##################################################
# Nonparametric Regression Models
##################################################

#--------------------------------------------------
# Nonparametric model for Width
#--------------------------------------------------

# Plot a scattergraph in terms of the original variable.

fig_file_name <- 'dev_vs_width.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(flyreels[, 'Width'],
     flyreels[, 'log_Price_resid_width'],
     main = 'Linear Model for Fly Reel Prices',
     xlab = 'Width',
     ylab = 'Deviation of Log Fly Reel Prices',
     col = 'blue')

# Add a line for the linear prediction from above.
points(flyreels[, 'Width'],
       predict(lm_width_fwl),
       lwd = 3, col = 'red')

dev.off()


# Estimate and plot nonparametric model for Width

np_width_fit_1 <- loess(log_Price_resid_width ~ width_resid,
                        flyreels,
                      span = 0.25)
# Calculate the predictions.
flyreels[, 'width_np'] <- np_width_fit_1$fitted


# Plot this curve width a scattergraph
# along the excess explanatory variable.


fig_file_name <- 'dev_np_vs_width_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(flyreels[, 'width_resid'],
     flyreels[, 'log_Price_resid_width'],
     main = 'Nonparametric Model for Fly Reel Prices',
     xlab = 'Deviation of Width',
     ylab = 'Deviation of Log Fly Reel Prices',
     col = 'blue')

# Add a line for the linear prediction from above.
points(flyreels[, 'width_resid'],
       predict(lm_width_fwl),
       lwd = 3, col = 'red')

# Add a line for the nonparametric prediction.
points(flyreels[, 'width_resid'],
       np_width_fit_1$fitted,
       lwd = 3, col = 'green')

dev.off()


#--------------------------------------------------
# Nonparametric model for Diameter
#--------------------------------------------------

# Plot a scattergraph in terms of the original variable.

fig_file_name <- 'dev_vs_diameter.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(flyreels[, 'Diameter'],
     flyreels[, 'log_Price_resid_diameter'],
     main = 'Linear Model for Fly Reel Prices',
     xlab = 'Diameter',
     ylab = 'Deviation of Log Fly Reel Prices',
     col = 'blue')

# Add a line for the linear prediction from above.
points(flyreels[, 'Diameter'],
       predict(lm_diameter_fwl),
       lwd = 3, col = 'red')

dev.off()


# Estimate and plot nonparametric model for Width

np_diameter_fit_1 <- loess(log_Price_resid_diameter ~ diameter_resid,
                        flyreels,
                        span = 0.25)
# Calculate the predictions.
flyreels[, 'diameter_np'] <- np_diameter_fit_1$fitted


# Plot this curve width a scattergraph
# along the excess explanatory variable.


fig_file_name <- 'dev_np_vs_diameter_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(flyreels[, 'diameter_resid'],
     flyreels[, 'log_Price_resid_diameter'],
     main = 'Nonparametric Model for Fly Reel Prices',
     xlab = 'Deviation of Diameter',
     ylab = 'Deviation of Log Fly Reel Prices',
     col = 'blue')

# Add a line for the linear prediction from above.
points(flyreels[, 'diameter_resid'],
       predict(lm_diameter_fwl),
       lwd = 3, col = 'red')

# Add a line for the nonparametric prediction.
points(flyreels[, 'diameter_resid'],
       np_diameter_fit_1$fitted,
       lwd = 3, col = 'green')

dev.off()



#--------------------------------------------------
# Nonparametric model for Density
#--------------------------------------------------

# Plot a scattergraph in terms of the original variable.

fig_file_name <- 'dev_vs_density.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(flyreels[, 'Density'],
     flyreels[, 'log_Price_resid_density'],
     main = 'Linear Model for Fly Reel Prices',
     xlab = 'Density',
     ylab = 'Deviation of Log Fly Reel Prices',
     col = 'blue')

# Add a line for the linear prediction from above.
points(flyreels[, 'Density'],
       predict(lm_density_fwl),
       lwd = 3, col = 'red')

dev.off()


# Estimate and plot nonparametric model for Width

np_density_fit_1 <- loess(log_Price_resid_density ~ density_resid,
                        flyreels,
                        span = 0.25)
# Calculate the predictions.
flyreels[, 'density_np'] <- np_density_fit_1$fitted


# Plot this curve width a scattergraph
# along the excess explanatory variable.


fig_file_name <- 'dev_np_vs_density_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(flyreels[, 'density_resid'],
     flyreels[, 'log_Price_resid_density'],
     main = 'Nonparametric Model for Fly Reel Prices',
     xlab = 'Deviation of Density',
     ylab = 'Deviation of Log Fly Reel Prices',
     col = 'blue')

# Add a line for the linear prediction from above.
points(flyreels[, 'density_resid'],
       predict(lm_density_fwl),
       lwd = 3, col = 'red')

# Add a line for the nonparametric prediction.
points(flyreels[, 'density_resid'],
       np_density_fit_1$fitted,
       lwd = 3, col = 'green')

dev.off()




# For each of the variables, it looks as though
# the linear model might be close enough.
# Nonetheless, we will fit a semiparametric model.



##################################################
# Semiparametric Models
##################################################

#--------------------------------------------------
# Revisit our best parametric model
# Model 6: Linear model for log of dollar sale price
# With interaction of Sealed and Made_in_USA
#--------------------------------------------------

target_var <- 'log_Price'

# Specify list of variables.
var_list <- c('Width', 'Diameter', 'Density',
              'Sealed', 'Machined', 'made_in_USA',
              'made_in_USA*Sealed')

lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_6 <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_6))


#--------------------------------------------------
# Compare this with a semi-parametric model
# with a nonparametric fit on width.
#--------------------------------------------------

# Specify list of variables.
var_list <- c(
  # 'Width',
  'width_np',
  'Diameter',
  # 'diameter_np',
  'Density',
  # 'density_np',
  'Sealed', 'Machined', 'made_in_USA',
  'made_in_USA*Sealed')

lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_sp_width <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_sp_width))


#--------------------------------------------------
# Compare this with a semi-parametric model
# with a nonparametric fit on diameter.
#--------------------------------------------------

# Specify list of variables.
var_list <- c(
  'Width',
  # 'width_np',
  # 'Diameter',
  'diameter_np',
  'Density',
  # 'density_np',
  'Sealed', 'Machined', 'made_in_USA',
  'made_in_USA*Sealed')

lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_sp_diameter <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_sp_diameter))


#--------------------------------------------------
# Compare this with a semi-parametric model
# with a nonparametric fit on density.
#--------------------------------------------------

# Specify list of variables.
var_list <- c(
  'Width',
  # 'width_np',
  'Diameter',
  # 'diameter_np',
  # 'Density',
  'density_np',
  'Sealed', 'Machined', 'made_in_USA',
  'made_in_USA*Sealed')

lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_sp_density <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_sp_density))


#--------------------------------------------------
# Compare this with a semi-parametric model
# with a nonparametric fit on all three
# continuous variables.
#--------------------------------------------------

# Specify list of variables.
var_list <- c(
  # 'Width',
  'width_np',
  # 'Diameter',
  'diameter_np',
  # 'Density',
  'density_np',
  'Sealed', 'Machined', 'made_in_USA',
  'made_in_USA*Sealed')

lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_sp_full <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_sp_full))

# Again, even with this aggressive step of including all three
# variables in a semiparametric form,
# the fit is still only slightly better and the model is very similar.


# Print the output to a LaTeX file.
tab_file_name <- 'reg_semipar.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_6,
                lm_sp_width,
                lm_sp_diameter,
                lm_sp_density,
                lm_sp_full),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_semipar',
       caption = "Semiparametric Models for Fly Reel Prices")






##################################################
# Generalized Additive Model
##################################################

# Now consider a semiparametric model using an
# estimation method that accounts for the joint estimation
# of the nonparametric functions and the parameters.
# This form of model is termed a Generalized Additive Model (GAM)
# and can be estimated with the mgcv package.

# Begin with the linear model specification.
var_list <- c('Width', 'Diameter', 'Density',
              'Sealed', 'Machined', 'made_in_USA',
              'made_in_USA*Sealed')
gam_fmla <- as.formula(sprintf('%s ~ %s',
                               target_var,
                              paste(var_list, collapse = ' + ')))

gam_model_lin <- gam(gam_fmla, data = flyreels)
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
var_list <- c('s(Width)', 's(Diameter)', 's(Density)',
              'Sealed', 'Machined', 'made_in_USA',
              'made_in_USA*Sealed')
gam_fmla <- as.formula(sprintf('%s ~ %s',
                               target_var,
                               paste(var_list, collapse = ' + ')))

gam_model_full <- gam(gam_fmla, data = flyreels)
print(summary(gam_model_full))



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
