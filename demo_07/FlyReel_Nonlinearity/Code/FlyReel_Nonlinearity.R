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
# FlyReel_Nonlinearity gives examples of OLS regression models
#   by considering a number of different model specifications.
# In this example, the model specification choices
#   use a parametric form to account to account for the nonlinearity.
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
# wd_path <- '~/GitHub/QMB6912S23/demo_07/FlyReel_Nonlinearity'
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


# Create logarithm of dependent variable.
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
# Introduce nonlinear interactions.
##################################################

# Perhaps the density of a reel matters more than the size.
# That is, it is an interaction between the weight and the size
# of the reel.


# Create a density variable.
colnames(flyreels)
flyreels[, 'Volume'] <- pi * (flyreels[, 'Diameter']/2)^2 * flyreels[, 'Width']
flyreels[, 'Density'] <- flyreels[, 'Weight'] / flyreels[, 'Volume']



##################################################
# Revisiting previous variable reduction
##################################################


# As a result of earlier analysis.
# Set target variable as the log transformation
# for remaining analysis.
target_var <- 'log_Price'


var_list <- c('Width', 
              'Diameter', 
              'Density',
              'Weight',
              'Sealed', 'Machined', 'made_in_USA')


#--------------------------------------------------
# Regression on Logarithm of Fly Reel Prices
#--------------------------------------------------

lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_log_all <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_model_log_all))


#--------------------------------------------------
# Reduced models
#--------------------------------------------------


# Notice that density, diameter and weight are highly
# correlated because they are functionally related.
# At least one should be excluded.

# Drop width.
var_list <- c(# 'Width', 
  'Diameter', 
  'Density',
  'Weight',
  'Sealed', 'Machined', 'made_in_USA')

lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_log_red_1 <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_model_log_red_1))


# Drop diameter.
var_list <- c('Width', 
  # 'Diameter', 
  'Density',
  'Weight',
  'Sealed', 'Machined', 'made_in_USA')

lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_log_red_2 <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_model_log_red_2))

# Drop weight.
var_list <- c('Width', 
  'Diameter',
  'Density',
  # 'Weight',
  'Sealed', 'Machined', 'made_in_USA')

lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_log_red_3 <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_model_log_red_3))


# Seems as though the weight variable can be excluded.
# Drop both weight and width.
var_list <- c(# 'Width', 
              'Diameter',
              'Density',
              # 'Weight',
              'Sealed', 'Machined', 'made_in_USA')

lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_log_red_4 <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_model_log_red_4))


tab_file_name <- 'reg_w_density.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_log_all, 
                lm_model_log_red_1, 
                lm_model_log_red_2, 
                lm_model_log_red_3,
                lm_model_log_red_4),
       file = out_file_name,
       label = 'tab:reg_w_density',
       caption = "Regression Models with Density Variable")




# Might keep width since the significance is marginal 
# and may become significant under a refined specification.


##################################################
# Estimating Regression Models
# by Country of Manufacture
##################################################

#--------------------------------------------------
# Regression for Sample Made in the USA
#--------------------------------------------------


# New variable list without made_in_USA indicator
# since it is redundant in separate samples.
var_list <- c('Width', 'Diameter', 'Density',
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
# but Machined is included for reels made in asia.
var_list <- c('Width', 'Diameter', 'Density',
              'Sealed')


lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_USA <- lm(data = flyreels[flyreels[, 'made_in_USA'] == TRUE, ],
                 formula = lm_fmla)
print(summary(lm_model_USA))



# Above variable list without made_in_USA indicator
# since it is redundant in separate samples,
# but Machined is included for reels made in Asia.
var_list <- c('Width', 'Diameter', 'Density',
              'Sealed', 'Machined')



lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_Asia <- lm(data = flyreels[flyreels[, 'made_in_USA'] == FALSE, ],
                 formula = lm_fmla)
print(summary(lm_model_Asia))



#--------------------------------------------------
# Print the output to a LaTeX file.
#--------------------------------------------------



tab_file_name <- 'reg_by_country.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_USA,
                lm_model_Asia),
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
RSS_unconstrained <- sum(lm_model_USA$residuals^2) +
  sum(lm_model_Asia$residuals^2)
print("RSS_unconstrained:")
print(RSS_unconstrained)
#
# The constrained RSS is calculated from the model
# that includes only the made_in_USA indicator:
RSS_constrained <- sum(lm_model_log_red_3$residuals^2)
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

# Check number of parameters in each model. 
summary(lm_model_USA) # 5 parameters.
summary(lm_model_Asia) # 6 parameters.
# Usually these are the same:
# num_vars <- 2*6
# but, in this case, all American reels are machioned, 
# so that model does not have a coefficient for machined.
num_vars <- 5 + 6

# To find the number of restrictions, count the parameters
# in the model on the full sample.
summary(lm_model_log_red_3) # 7 parameters.
# A test of eight restrictions
# (one for each variable minus the interaction).
num_restr <- 11 - 7

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
var_list <- c('Width', 'Diameter', 'Density',
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
# Interaction with Density and made_in_USA
#--------------------------------------------------

# Specify list of variables with interactions.
var_list <- c('Width', 'Diameter', 'Density',
              'Sealed', 'Machined', 'made_in_USA',
              'made_in_USA*Density')

lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_int_2 <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_model_int_2))


#--------------------------------------------------
# Interactions with made_in_USA with Sealed and Density.
#--------------------------------------------------

# Specify list of variables with interactions.
var_list <- c('Width', 'Diameter', 'Density',
              'Sealed', 'Machined', 'made_in_USA',
              'made_in_USA*Sealed', 'made_in_USA*Density')

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
texreg(l = list(lm_model_int_1,
                lm_model_int_2,
                lm_model_int_3),
       file = out_file_name,
       label = 'tab:reg_interactions',
       caption = "Regression Models with Interaction Terms by Country of Manufacture")

# The interaction with sealed and country of manufacture is significant.

# Since all variables are significant in this model and it
# has the highest R-squared, this is the recommended model.

##################################################
# End
##################################################
