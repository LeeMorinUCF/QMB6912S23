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
# FlyReel_SampleSelection gives examples of
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
# wd_path <- '~/GitHub/QMB6912S23/demo_10/FlyReel_SampleSelection'
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

# # Generate new dependent variable with results from Problem Set 6.
# # First define the Box-Cox transformation.
# Lambda_Price <- function(price, lambda) {
#
#   if (lambda == 0) {
#     return(log(price))
#   } else {
#     return((price^lambda - 1)/lambda)
#   }
#
# }
#
# # Recall the optimal exponent from the MLE.
# lambda_hat <- 0.43
# flyreels[, 'Trans_Price'] <- Lambda_Price(price = flyreels[, 'Price'],
#                                           lambda = lambda_hat)


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
# Separate Linear Regression Models
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

lm_model_4 <- lm(data = flyreels[flyreels[, 'made_in_USA'] == TRUE, ],
                 formula = lm_fmla)
print(summary(lm_model_4))


# Eliminate Width variable, which is not significant
# in this smaller sample.
var_list <- c(# 'Width',
              'Diameter', 'Density',
              'Sealed')



lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_4_2 <- lm(data = flyreels[flyreels[, 'made_in_USA'] == TRUE, ],
                 formula = lm_fmla)
print(summary(lm_model_4_2))




#--------------------------------------------------
# Regression for Sample Made Outside the USA
#--------------------------------------------------



# Above variable list without made_in_USA indicator
# since it is redundant in separate samples,
# but Machined is included for reels made in Asia.
var_list <- c('Width', 'Diameter', 'Density',
              'Sealed', 'Machined')



lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_5 <- lm(data = flyreels[flyreels[, 'made_in_USA'] == FALSE, ],
                 formula = lm_fmla)
print(summary(lm_model_5))


# Eliminate Width variable, which is not significant
# in this smaller sample.
var_list <- c(# 'Width',
              'Diameter', 'Density',
              'Sealed', 'Machined')



lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_5_2 <- lm(data = flyreels[flyreels[, 'made_in_USA'] == FALSE, ],
                 formula = lm_fmla)
print(summary(lm_model_5_2))


#--------------------------------------------------
# Print the output to a LaTeX file.
#--------------------------------------------------


tab_file_name <- 'reg_by_country.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_4,
                lm_model_4_2,
                lm_model_5,
                lm_model_5_2),
       file = out_file_name,
       label = 'tab:reg_by_country',
       caption = "Regression Models by Country of Manufacture")

##################################################
# Create Dependent Variables for
# Sample selection Models
##################################################

# The selection function requires each model
# (America vs other countries) to be
# specified with a separate variable.

# Generate dependent variable in the outcome equation.
# Leave only what is observed.
flyreels[, sprintf('%s_other', target_var)] <-
  flyreels[, target_var] *
  (flyreels[, 'made_in_USA'] == 0)
flyreels[, sprintf('%s_USA', target_var)] <-
  flyreels[, target_var] *
  (flyreels[, 'made_in_USA'] == 1)




##################################################
# Summary of Variables by Country of Manufacture
# to Investigate Sample Selection
##################################################


# As a preliminary step, compare the distributions of
# explanatory variables by country of manufacture.

# Compare continuous variables.
summary(flyreels[flyreels[, 'made_in_USA'] == 1,
                      c('Weight', 'Diameter', 'Width',
                        'Volume', 'Density')])
summary(flyreels[flyreels[, 'made_in_USA'] == 0,
                 c('Weight', 'Diameter', 'Width',
                   'Volume', 'Density')])
# Not much difference, so not much promise to indicate
# the country of manufacture from these variables.

# Compare categorical variables.
summary(flyreels[flyreels[, 'made_in_USA'] == 1,
                 c('Sealed', 'Machined')])
summary(flyreels[flyreels[, 'made_in_USA'] == 0,
                 c('Sealed', 'Machined')])
# All American reels are machined but about equal numbers
# are sealed and unsealed.
# In Asia, the majority are machined, while around 30% are cast,
# however, more than two thirds of Asian reels are sealed.

# Use these in the probit model and selection model below.




##################################################
# Probit Models to Investigate Sample Selection
##################################################

# Now estimate a probit model to predict the selection indicator.
# Start with all the other variables in the model.
probit_var_list <- c('Weight', 'Diameter', 'Width',
                     'Volume', 'Density',
                     'Sealed', 'Machined')


probit_var <- 'made_in_USA'
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              probit_var,
                              paste(probit_var_list, collapse = ' + ')))

tobit_5_sel_probit1 <- glm(formula = lm_fmla,
                           data = flyreels,
                           family = binomial(link = "probit"))

summary(tobit_5_sel_probit1)


# Estimate a reduced model.
# Eliminating variables one-by-one, I estimated the following model.
probit_var_list <- c(
  'Width',
  # 'Diameter',
  # 'Volume',
  'Weight',
  'Density',
  'Sealed' # ,
  # 'Machined'
)

probit_var <- 'made_in_USA'
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              probit_var,
                              paste(probit_var_list, collapse = ' + ')))

tobit_5_sel_probit2 <- glm(formula = lm_fmla,
                           data = flyreels,
                           family = binomial(link = "probit"))

summary(tobit_5_sel_probit2)
# After some variable reduction,
# Width, Weight, Density and Sealed are useful to indicate
# selection into country of manufacture.


# This should provide a concise but useful model to
# indicate the fly reel characteristics that would be
# valued differently depending on the country of manufacture.


# Print the output to a LaTeX file.
tab_file_name <- 'reg_probit.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(tobit_5_sel_probit1,
                tobit_5_sel_probit2),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_probit',
       caption = "Probit Models for Country-of-Manufacture Selection of Fly Reels")



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
# for both observation equations, American and others.

# Specify Selection formula.
probit_var_list <- c(
  'Width',
  # 'Diameter',
  # 'Volume',
  'Weight',
  'Density',
  'Sealed' # ,
  # 'Machined'
)

probit_var <- 'made_in_USA'
sel_fmla <- as.formula(sprintf('%s ~ %s',
                              probit_var,
                              paste(probit_var_list, collapse = ' + ')))

# Specify observation equation for fly reels
# made either inside or outside the USA.

USA_var_list <- c('Width', 'Diameter', 'Density',
                  'Sealed')

USA_fmla <- as.formula(sprintf('%s ~ %s',
                                 sprintf('%s_USA', target_var),
                                 paste(USA_var_list, collapse = ' + ')))

other_var_list <- c('Width', 'Diameter', 'Density',
                    'Sealed', 'Machined')

other_fmla <- as.formula(sprintf('%s ~ %s',
                                 sprintf('%s_other', target_var),
                                 paste(other_var_list, collapse = ' + ')))


tobit_5_sel_1 <-
  selection(selection = sel_fmla,
            outcome = list(other_fmla,
                           USA_fmla),
            iterlim = 20,
            # method = '2step',
            data = flyreels)


summary(tobit_5_sel_1)

# Notice the warning message:
# 1: In sqrt(diag(vc)) : NaNs produced
# 2: In sqrt(diag(vc)) : NaNs produced
# 3: In sqrt(diag(vcov(object, part = "full"))) : NaNs produced

# This suggests that the likelihood function is flat in some areas
# of the parameter space.

# This model has its imperfections but is a good start.
# Several variables are statistically insignificant
# but these can be removed one by one
# to produce a refined model.

# The goal will be to obtain a final model that has
# well-defined standard errors for all variables.



#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate reduced model:
# Eliminate Diameter from other-country equation
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.
# Eliminate variables from the observation equation.

# Specify Selection formula.
probit_var_list <- c(
  'Width',
  # 'Diameter',
  # 'Volume',
  'Weight',
  'Density',
  'Sealed' # ,
  # 'Machined'
)

probit_var <- 'made_in_USA'
sel_fmla <- as.formula(sprintf('%s ~ %s',
                               probit_var,
                               paste(probit_var_list, collapse = ' + ')))

# Specify observation equation for fly reels
# made either inside or outside the USA.

USA_var_list <- c(
  'Width',
  'Diameter',
  'Density',
  'Sealed'
)

USA_fmla <- as.formula(sprintf('%s ~ %s',
                               sprintf('%s_USA', target_var),
                               paste(USA_var_list, collapse = ' + ')))

other_var_list <- c(
  'Width',
  # 'Diameter', # Remove this insignificant variable and re-estimate.
  'Density',
  'Sealed',
  'Machined'
)

other_fmla <- as.formula(sprintf('%s ~ %s',
                                 sprintf('%s_other', target_var),
                                 paste(other_var_list, collapse = ' + ')))



tobit_5_sel_2 <-
  selection(selection = sel_fmla,
            outcome = list(other_fmla,
                           USA_fmla),
            iterlim = 20,
            # method = '2step',
            data = flyreels)


summary(tobit_5_sel_2)

#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate reduced model:
# After eliminating Diameter from other-country equation,
# eliminate Width from American equation.
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.
# Eliminate variables from the observation equation.

# Specify Selection formula.
probit_var_list <- c(
  'Width',
  # 'Diameter',
  # 'Volume',
  'Weight',
  'Density',
  'Sealed' # ,
  # 'Machined'
)

probit_var <- 'made_in_USA'
sel_fmla <- as.formula(sprintf('%s ~ %s',
                               probit_var,
                               paste(probit_var_list, collapse = ' + ')))

# Specify observation equation for fly reels
# made either inside or outside the USA.

USA_var_list <- c(
  # 'Width', # Remove this insignificant variable and re-estimate.
  'Diameter',
  'Density',
  'Sealed'
)

USA_fmla <- as.formula(sprintf('%s ~ %s',
                               sprintf('%s_USA', target_var),
                               paste(USA_var_list, collapse = ' + ')))

other_var_list <- c(
  'Width',
  # 'Diameter', # Remove this insignificant variable and re-estimate.
  'Density',
  'Sealed',
  'Machined'
)

other_fmla <- as.formula(sprintf('%s ~ %s',
                                 sprintf('%s_other', target_var),
                                 paste(other_var_list, collapse = ' + ')))



tobit_5_sel_3 <-
  selection(selection = sel_fmla,
            outcome = list(other_fmla,
                           USA_fmla),
            iterlim = 20,
            # method = '2step',
            data = flyreels)


summary(tobit_5_sel_3)


#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate reduced model:
# After eliminating Diameter from other-country equation,
# and eliminating Width from American equation,
# eliminate Weight from the selection equation.
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.
# Now eliminate variables from the selection equation.

# Specify Selection formula.
probit_var_list <- c(
  'Width',
  # 'Diameter',
  # 'Volume',
  # 'Weight', # Remove this insignificant variable and re-estimate.
  'Density',
  'Sealed' # ,
  # 'Machined'
)

probit_var <- 'made_in_USA'
sel_fmla <- as.formula(sprintf('%s ~ %s',
                               probit_var,
                               paste(probit_var_list, collapse = ' + ')))

# Specify observation equation for fly reels
# made either inside or outside the USA.

USA_var_list <- c(
  # 'Width', # Remove this insignificant variable and re-estimate.
  'Diameter',
  'Density',
  'Sealed'
)

USA_fmla <- as.formula(sprintf('%s ~ %s',
                               sprintf('%s_USA', target_var),
                               paste(USA_var_list, collapse = ' + ')))

other_var_list <- c(
  'Width',
  # 'Diameter', # Remove this insignificant variable and re-estimate.
  'Density',
  'Sealed',
  'Machined'
)

other_fmla <- as.formula(sprintf('%s ~ %s',
                                 sprintf('%s_other', target_var),
                                 paste(other_var_list, collapse = ' + ')))



tobit_5_sel_4 <-
  selection(selection = sel_fmla,
            outcome = list(other_fmla,
                           USA_fmla),
            iterlim = 20,
            # method = '2step',
            data = flyreels)


summary(tobit_5_sel_4)

# This is the first model that is numerically
# well behaved but it still contains
# some variables that are statistically significant.

#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate reduced model:
# After eliminating Diameter from other-country equation,
# eliminating Width from American equation,
# and eliminating Weight from the selection equation,
# next, eliminate Density from the selection equation.
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.
# Now eliminate variables from the selection equation.

# Specify Selection formula.
probit_var_list <- c(
  'Width',
  # 'Diameter',
  # 'Volume',
  # 'Weight', # Remove this insignificant variable and re-estimate.
  # 'Density',
  'Sealed' # ,
  # 'Machined'
)

probit_var <- 'made_in_USA'
sel_fmla <- as.formula(sprintf('%s ~ %s',
                               probit_var,
                               paste(probit_var_list, collapse = ' + ')))

# Specify observation equation for fly reels
# made either inside or outside the USA.

USA_var_list <- c(
  # 'Width', # Remove this insignificant variable and re-estimate.
  'Diameter',
  'Density',
  'Sealed'
)

USA_fmla <- as.formula(sprintf('%s ~ %s',
                               sprintf('%s_USA', target_var),
                               paste(USA_var_list, collapse = ' + ')))

other_var_list <- c(
  'Width',
  # 'Diameter', # Remove this insignificant variable and re-estimate.
  'Density',
  'Sealed',
  'Machined'
)

other_fmla <- as.formula(sprintf('%s ~ %s',
                                 sprintf('%s_other', target_var),
                                 paste(other_var_list, collapse = ' + ')))



tobit_5_sel_5 <-
  selection(selection = sel_fmla,
            outcome = list(other_fmla,
                           USA_fmla),
            iterlim = 20,
            # method = '2step',
            data = flyreels)


summary(tobit_5_sel_5)

# Again, this model is well-behaved numerically
# One statistically insignificant variable remains.

#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate reduced model:
# After eliminating Diameter from other-country equation,
# eliminating Width from American equation,
# and eliminating Weight and Density from the selection equation,
# next, eliminate Width from the selection equation.
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.
# Now eliminate variables from the selection equation.

# Specify Selection formula.
probit_var_list <- c(
  # 'Width', # Remove this insignificant variable and re-estimate.
  # 'Diameter',
  # 'Volume',
  # 'Weight', # Remove this insignificant variable and re-estimate.
  # 'Density',
  'Sealed' # ,
  # 'Machined'
)

probit_var <- 'made_in_USA'
sel_fmla <- as.formula(sprintf('%s ~ %s',
                               probit_var,
                               paste(probit_var_list, collapse = ' + ')))

# Specify observation equation for fly reels
# made either inside or outside the USA.

USA_var_list <- c(
  # 'Width', # Remove this insignificant variable and re-estimate.
  'Diameter',
  'Density',
  'Sealed'
)

USA_fmla <- as.formula(sprintf('%s ~ %s',
                               sprintf('%s_USA', target_var),
                               paste(USA_var_list, collapse = ' + ')))

other_var_list <- c(
  'Width',
  # 'Diameter', # Remove this insignificant variable and re-estimate.
  'Density',
  'Sealed',
  'Machined'
)

other_fmla <- as.formula(sprintf('%s ~ %s',
                                 sprintf('%s_other', target_var),
                                 paste(other_var_list, collapse = ' + ')))



tobit_5_sel_6 <-
  selection(selection = sel_fmla,
            outcome = list(other_fmla,
                           USA_fmla),
            iterlim = 20,
            # method = '2step',
            data = flyreels)
# Error in if (rho1 <= -1) rho1 <- -0.99 :
#   missing value where TRUE/FALSE needed
# In addition: Warning messages:
#   1: In heckit5fit(selection, as.formula(formula1), as.formula(formula2),  :
#     Inverse Mills Ratio is virtually multicollinear to the rest of explanatory variables in the outcome equation 1
#   2: In heckit5fit(selection, as.formula(formula1), as.formula(formula2),  :
#     Inverse Mills Ratio is virtually multicollinear to the rest of explanatory variables in the outcome equation 2

# With these numerical problems,
# it is better to keep the additional variable
# in the selection equation,
# even though it may be statistically insignificant.
# Notice that the remaining selection variable Sealed
# appears in both observation equations and there is no
# variable in the selection equation that is excluded.
# The other extreme would offer better performance,
# that is, having some variables in the selection
# equation that are not included in the observation equations.

# summary(tobit_5_sel_6)


# Revert back to model 5 to analyze the differences
# between country of manufacture.

summary(tobit_5_sel_5)

# Print the output to a LaTeX file.
tab_file_name <- 'tobit_5_sel.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(tobit_5_sel_1,
                tobit_5_sel_2,
                tobit_5_sel_3,
                tobit_5_sel_4,
                tobit_5_sel_5),
       digits = 5,
       fontsize = 'small', # So table fits on page.
       file = out_file_name,
       label = 'tab:tobit_5_sel',
       caption = "Selection Models for Fly Reel Prices")

# First of all, this confirms that machined reels
# are more valuable, with a coefficient of 0.76, instead of 0.63,
# as was found in the separate linear regression model.
# It also justifies the fact that only machined reels
# are produced in the USA.
# This could relate to some advantage in American production
# technology or, rather, the outdated casting techniques
# used for cheaper reels produced overseas.

# In the model for American-made reels,
# the coefficients on all three variables are statistically
# the same as those in the separate linear regression model.

# For the fly reels made overseas, however,
# Width replaces diameter as a proxy for the size of the reels.
# As a consequence, Density is a relatively less valuable feature.
# The value of sealed reels, however, is even greater,
# after accounting for the selection of different production techniques
# jointly with manufacturing location.
# The coefficient on sealed reels jumps to 0.90, compared to 0.65
# in the linear model.
# This suggests a higher premium than indicated earlier,
# once we also consider the choice of country of manufacture.
# Similarly, the value of machined reels rises from 0.65 to 0.76
# in the selection model, suggesting an even higher
# for this design when produced overseas.


# In conclusion, an American fly reel producer should not consider
# producing cast reels, unless the purpose is to explore the change in value.
# I suspect, however, that this outcome has been observed in
# the past, so the older production techniques have been abandoned for a reason.
# Machined reels are also more valuable when produced overseas,
# so a company has to compare the difference in labor costs with the relative
# cost of producing machined reels overseas.

# Similarly, sealed reels produce a much higher premium overseas
# than was originally estimated with the linear model.
# Reels produced overseas should be sealed, unless the cost of
# changing the manufacturing process would outweigh this premium.
# Likewise for the American reels, except the premium is one third the size.

# However it is measured, the size of a reel matters:
# bigger reels are more valuable.
# A manufacturer can compare the cost of materials with
# the premiums attached to those dimensions when producing a reel.

# Perhaps the largest difference is in the intercept term:
# 1.07 overseas vs 2.14 in the USA, double the size.
# In the linear model, the intercepts were 2.41 vs 3.48,
# which measures a similar percentage difference.
# No matter how it is measured, there exists a substantial premium
# for fly reels made in the USA.


##################################################
# End
##################################################
