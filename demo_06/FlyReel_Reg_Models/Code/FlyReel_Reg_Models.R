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
# FlyReel_Reg_Models gives examples of OLS regression models
#   by considering a number of different model specifications.
# In this example, the model specification choices
#   have a parametric form.
#
# Dependencies:
#   Libraries xtable and texreg
#   to print tables of regression results.
#
#
##################################################


##################################################
# Preparing the Workspace
##################################################


# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory, if running interactively.
# wd_path <- '~/GitHub/QMB6912S23/demo_06/FlyReel_Reg_Models'
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

# Calculate logarithm of dependent variable.
flyreels[, 'log_Price'] <- log(flyreels[, 'Price'])



# Set categorical variables as factors.
cat_var_list <- colnames(flyreels)[lapply(flyreels, class) == "character"]
for (var_name in cat_var_list) {
  flyreels[, var_name] <- as.factor(flyreels[, var_name])
}

# Initial inspection.
print('FlyReels Dataset with Categorical Factors:')
print(summary(flyreels))


# Replace Country Indicator with made_in_USA Indicator.
table(flyreels[, 'Country'], useNA = 'ifany')
flyreels[, 'made_in_USA'] <- flyreels[, 'Country'] == 'USA'
# Check:
table(flyreels[, 'Country'],
      flyreels[, 'made_in_USA'], useNA = 'ifany')




##################################################
# Analyze Dependent Variable
##################################################



# Kernel-smoothed pdf of the (un-transformed) price.
density_price <- density(flyreels[, 'Price'])
fig_file_name <- 'density_prices.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(density_price,
     main = 'Kernel-Smoothed pdf of Fly Reel Prices',
     xlab = 'Price',
     col = 'blue',
     lwd = 3)
dev.off()


# Kernel-smoothed pdf of the natural logarithm of price.
density_log_price <- density(flyreels[, 'log_Price'])
fig_file_name <- 'density_log_prices.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(density_log_price,
     main = 'Kernel-Smoothed pdf of the Natural Log. of Fly Reel Prices',
     xlab = 'Logarithm of Price',
     col = 'blue',
     lwd = 3)
dev.off()




##################################################
# Box-Cox Transformation
##################################################

#--------------------------------------------------
# Consider the model with different values
# of the exponent parameter
# for the Box-Cox transformation
#--------------------------------------------------


# Box-Cox transformation.
BoxCox_Trans <- function(price, lambda) {

  if (lambda == 0) {
    return(log(price))
  } else {
    return((price^lambda - 1)/lambda)
  }

}


# Create the likelihood function.
log_like_uni <- function(price, lambda) {

  n <- length(price)
  BoxCox_Trans <- BoxCox_Trans(price, lambda)
  mu_0_lambda <- mean(BoxCox_Trans)
  sigma_2_lambda <- sum((BoxCox_Trans - mu_0_lambda)^2)/n

  like <- - n/2*log(2*pi*sigma_2_lambda)
  like <- like - 1/2/sigma_2_lambda*sum((BoxCox_Trans - mu_0_lambda)^2)
  like <- like + (lambda - 1)*sum(log(price))

  return(like)

}

# Calculate values of the log-likelihood function.
lambda_grid <- seq(-1, 2.5, by = 0.001)
like_grid <- 0*lambda_grid
for (lambda_num in 1:length(lambda_grid)) {
  like_grid[lambda_num] <- log_like_uni(price = flyreels[, 'Price'],
                                        lambda = lambda_grid[lambda_num])
}

# Find the MLE, the unrestricted estimate.
lambda_hat <- lambda_grid[which.max(like_grid)]
like_MLE <- max(like_grid)



# Generate new dependent variable with results from Problem Set 6.
flyreels[, 'Trans_Price'] <- BoxCox_Trans(price = flyreels[, 'Price'],
                                          lambda = lambda_hat)


# Now plot the density of this transformed variable.

# Kernel-smoothed pdf of the Box-Cox Transformation of price.
density_trans_price <- density(flyreels[, 'Trans_Price'])
fig_file_name <- 'density_trans_prices.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(density_trans_price,
     main = 'Kernel-Smoothed pdf of the Box-Cox Transformation of Fly Reel Prices',
     xlab = 'Logarithm of Price',
     col = 'blue',
     lwd = 3)
dev.off()


##################################################
# Estimating Regression Models
# by Transformation of the Dependent Variable
##################################################


#--------------------------------------------------
# Regression on (Un-transformed) Fly Reel Prices
#--------------------------------------------------

var_list <- c('Width', 'Diameter', 'Weight',
              'Sealed', 'Machined', 'made_in_USA')

target_var <- 'Price'
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_price <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_model_price))

#--------------------------------------------------
# Regression on Logarithm of Fly Reel Prices
#--------------------------------------------------

target_var <- 'log_Price'
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_log <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_model_log))


#--------------------------------------------------
# Regression on Box-Cox transformation of Fly Reel Prices
#--------------------------------------------------


target_var <- 'Trans_Price'
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_trans <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_model_trans))





#--------------------------------------------------
# Print the output to a LaTeX file.
#--------------------------------------------------



tab_file_name <- 'reg_by_dep_var.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_price,
                lm_model_log,
                lm_model_trans),
       custom.model.names = c('Price', 'Log. Price', 'Box-Cox'),
       file = out_file_name,
       label = 'tab:reg_by_dep_var',
       caption = "Regression Models with Different Dependent Variables")


# Although the model built on the original price levels
# has statistically significant coefficients,
# the two transformed models have a better fit,
# with higher values of R-squared.
# Since the difference between the latter two models is marginal,
# it is better to model the logarithm of fly reel prices,
# which has the added advantage of interpretability
# of the coefficients,
# which approximately represent percentage changes in fly reel prices.


# Set target variable as the log transformation
# for remaining analysis.
target_var <- 'log_Price'


#--------------------------------------------------
# Estimate a reduced model excluding the insignificant variables.
#--------------------------------------------------

var_list <- c(# 'Width', 
              # 'Diameter', 
              'Weight',
              'Sealed', 'Machined', 'made_in_USA')

lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_log_red_1 <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_model_log_red_1))



##################################################
# Estimating Regression Models
# by Country of Manufacture
##################################################

#--------------------------------------------------
# Regression for Sample Made in the USA
#--------------------------------------------------


# New variable list without made_in_USA indicator
# since it is redundant in separate samples.
var_list <- c('Width', 'Diameter', 'Weight',
              'Sealed', 'Machined')

# Consider relationships of variables with
# country of manufacture.
table(flyreels[, 'Machined'],
      flyreels[, 'made_in_USA'], useNA = 'ifany')
# All American reels are machined.
table(flyreels[, 'Sealed'],
      flyreels[, 'made_in_USA'], useNA = 'ifany')
# Sealed and unsealed reels are made in both regions.

# New variable list without made_in_USA indicator
# since it is redundant in separate samples,
# but Machined is included for reels made in Asia.
var_list <- c('Width', 'Diameter', 'Weight',
              'Sealed')

lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_USA <- lm(data = flyreels[flyreels[, 'made_in_USA'] == TRUE, ],
                 formula = lm_fmla)
print(summary(lm_model_USA))


# Estimate a reduced model on the made_in_USA sample.
var_list <- c(# 'Width', # 'Diameter', 
              'Weight',
              'Sealed')

lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_USA_red_1 <- lm(data = flyreels[flyreels[, 'made_in_USA'] == TRUE, ],
                   formula = lm_fmla)
print(summary(lm_model_USA_red_1))





# The above variable lists exclude the made_in_USA indicator
# since it is redundant in separate samples,
# but Machined is included for reels made in Asia.
var_list <- c('Width', 'Diameter', 'Weight',
              'Sealed', 'Machined')



lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_Asia <- lm(data = flyreels[flyreels[, 'made_in_USA'] == FALSE, ],
                 formula = lm_fmla)
print(summary(lm_model_Asia))


# Estimate a reduced model on the sample of reels made in Asia.
var_list <- c(# 'Width', 
              # 'Diameter', 
              'Weight',
              'Sealed', 'Machined')



lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_Asia_red_1 <- lm(data = flyreels[flyreels[, 'made_in_USA'] == FALSE, ],
                    formula = lm_fmla)
print(summary(lm_model_Asia_red_1))






#--------------------------------------------------
# Print the output to a LaTeX file.
#--------------------------------------------------



tab_file_name <- 'reg_by_country.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_log_red_1, 
                lm_model_USA,
                lm_model_USA_red_1,
                lm_model_Asia,
                lm_model_Asia_red_1),
       custom.model.names = c('USA 1', 'USA 2', 'USA 3', 
                              'Asia 1', 'Asia 2'),
       file = out_file_name,
       label = 'tab:reg_by_country',
       caption = "Regression Models by Country of Manufacture")


##################################################
#
# Test for separate coefficients by country of manufacture
#   An example of joint hypothesis testing.
print("Test for separate coefficients by country of manufacture")
#
# The unconstrained RSS is calculated from the models
# estimated separately by country of manufacture:
RSS_unconstrained <- sum(lm_model_USA_red_1$residuals^2) +
  sum(lm_model_Asia_red_1$residuals^2)
print("RSS_unconstrained:")
print(RSS_unconstrained)
#
# The constrained RSS is calculated from the model
# that includes only the made_in_USA indicator:
RSS_constrained <- sum(lm_model_log_red_1$residuals^2)
print("RSS_constrained:")
print(RSS_constrained)
#
# Follow the approach for conducting the F-test.
# Are the coefficients the same for fly reels
# made in the USA or elsewhere?
#
##################################################

# Need sample size and number of variables.

num_obs <- nrow(flyreels)
num_vars <- 2*4 # 4 coefficients in each model.


# A test of three restrictions
# (one for each variable minus the interaction).
num_restr <- 4 - 1

F_stat <- (RSS_constrained - RSS_unconstrained)/num_restr /
  RSS_unconstrained*(num_obs - num_vars)
print("F-statistic:")
print(F_stat)

# This value is greater than 1, so we can compare it to the critical value
# of the F-statistic at the specified degrees of freedom for
# a conventional level of significance.

# Calculate critical values.
F_critical_1 <- qf(p = 0.01,
                   df1 = num_restr, df2 = (num_obs - num_vars - 1),
                   lower.tail = FALSE)
F_critical_5 <- qf(p = 0.05,
                   df1 = num_restr, df2 = (num_obs - num_vars - 1),
                   lower.tail = FALSE)
F_critical_10 <- qf(p = 0.10,
                   df1 = num_restr, df2 = (num_obs - num_vars - 1),
                   lower.tail = FALSE)

print("Critical value of F-statistic:")
print("at the 1% level")
print(F_critical_1)
print("at the 5% level")
print(F_critical_5)
print("at the 10% level")
print(F_critical_10)


# This places the F-statistic between the critical values for the
# 5 and 10 percent levels of significance.
# Conclude that fly reel prices may have some difference by
# country of manufacture but the difference is marginal.
# This suggests little justification for separate models by
# country of manufacture.
# We can investigate small differences between the models.



##################################################
# Consider Interaction Terms
# for Partially Separate Model
##################################################


#--------------------------------------------------
# Interaction with Sealed and made_in_USA
#--------------------------------------------------

# Specify list of variables with interactions.
var_list <- c(# 'Width', 'Diameter', 
              'Weight',
              'Sealed', 'Machined', 'made_in_USA',
              'made_in_USA*Sealed')
# Notice that we keep the made_in_USA indicator
# to maintain different intercept by country.

lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_int_1 <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_model_int_1))


#--------------------------------------------------
# Interaction with Weight and made_in_USA
#--------------------------------------------------


# Specify list of variables with interactions.
var_list <- c(# 'Width', 'Diameter',
              'Weight',
              'Sealed', 'Machined', 'made_in_USA',
              'made_in_USA*Weight')

lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_int_2 <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_model_int_2))


#--------------------------------------------------
# Interactions with made_in_USA with Sealed and Weight.
#--------------------------------------------------

# Specify list of variables with interactions.
var_list <- c(# 'Width', 'Diameter',
              'Weight',
              'Sealed', 'Machined', 'made_in_USA',
              'made_in_USA*Sealed', 'made_in_USA*Weight')

lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_int_3 <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_model_int_3))




#--------------------------------------------------
# Print the output to a LaTeX file.
#--------------------------------------------------



tab_file_name <- 'reg_interactions.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_log_red_1, 
                lm_model_int_1,
                lm_model_int_2,
                lm_model_int_3),
       file = out_file_name,
       label = 'tab:reg_interactions',
       caption = "Regression Models with Interaction Terms by Country of Manufacture")

# The interaction with sealed and country of manufacture is significant.
# Since all variables are significant in this model and it
# has the highest R-squared, this is the recommended model.


#--------------------------------------------------
# Sensitivity Analysis
# Revisit excluded variables with recommended model.
#--------------------------------------------------


# Specify list of variables with interactions.
var_list <- c('Width', 
  # 'Diameter',
  'Weight',
  'Sealed', 'Machined', 'made_in_USA',
  'made_in_USA*Sealed')

lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_int_4 <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_model_int_4))




##################################################
# End
##################################################
