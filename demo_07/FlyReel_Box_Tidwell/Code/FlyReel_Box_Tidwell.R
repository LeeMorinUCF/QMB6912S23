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
# FlyReel_Box_Tidwell gives examples of additive
#   regression models augmented with a number of
#   different nonlinear model specifications,
#   using Box-Tidwell transformations.
#
# Dependencies:
#   Libraries xtable and texreg
#   to print tables of regression results.
#   car for fitting models with Box-Tidwell transformations.
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
# wd_path <- '~/GitHub/QMB6912S23/demo_07/FlyReel_Box_Tidwell'
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



# Create a density variable.
colnames(flyreels)
flyreels[, 'Volume'] <- pi * (flyreels[, 'Diameter']/2)^2 * flyreels[, 'Width']
flyreels[, 'Density'] <- flyreels[, 'Weight'] / flyreels[, 'Volume']





##################################################
# Linear Regression Model
##################################################

# In a previous exercise I recommended the following
# linear regression model.

#--------------------------------------------------
# Interaction with Sealed and made_in_USA
#--------------------------------------------------

# As determined in Problem Set 3, and confirmed in Problem set 6,
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
# Box-Tidwell Transformation
##################################################

# Model with each of the continuous variables
# available for transformation.

# Categorical variables are not suited to the Box-Tidwell
# transformation, unless they are numeric with ordinal relationship..

# Note that the box.tidwell() function is deprecated,
# it was replaced with boxTidwell().



#--------------------------------------------------
# Transformation of Width
#--------------------------------------------------


# bt_width <- boxTidwell(formula =
#                          log_Price ~
#                          Width, # +
#                        # Diameter +
#                        # Density,
#                        other.x = ~
#                          # Width +
#                          Diameter +
#                          Density +
#                          Sealed +
#                          Machined +
#                          made_in_USA +
#                          made_in_USA*Sealed,
#                        max.iter = 1000,
#                        tol = 0.001,
#                        verbose = TRUE,
#                        data = flyreels)
# Error in lm.fit(cbind(1, x1.p, x2), y, ...) : NA/NaN/Inf in 'x'

# This one failed, possibly because the optimization routine
# passed through a parameter value that rendered one of the
# density observations badly behaved.

# I decided to drop one variable to get around
# the numerical issues with the optimization.

bt_width <- boxTidwell(formula =
                         log_Price ~
                         Width, # +
                       # Diameter +
                       # Density,
                       other.x = ~
                         # Width +
                         # Diameter +
                         Density +
                         Sealed +
                         Machined +
                         made_in_USA +
                         made_in_USA*Sealed,
                       max.iter = 100,
                       tol = 0.001,
                       data = flyreels)

# The output is a test on the exponent.
print(bt_width)
# MLE of lambda Score Statistic (z) Pr(>|z|)
#        1.1615              0.1587   0.8739
#
# iterations =  5


# The exponent parameter is not
# statistically different from one.
# This supports a linear form for Width,
# confirming our result from the nonparametric analysis.

# Print the output to a LaTeX file.
tab_file_name <- 'bt_width.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_width)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)


#--------------------------------------------------
# Transformation of Width
#--------------------------------------------------


bt_diameter <- boxTidwell(formula =
                            log_Price ~
                            # Width +
                            Diameter, # +
                          # Density,
                          other.x = ~
                            Width +
                            # Diameter +
                            Density +
                            Sealed +
                            Machined +
                            made_in_USA +
                            made_in_USA*Sealed,
                          max.iter = 100,
                          tol = 0.001,
                          verbose = TRUE,
                          data = flyreels)

# The output is a test on the exponent.
print(bt_diameter)
# MLE of lambda Score Statistic (z) Pr(>|z|)
#      -0.35213             -1.2106    0.226
#
# iterations =  4

# This is very weak evidence for
# an inverse square root transformation
# for diameter but it is not estimated accurately.

# Print the output to a LaTeX file.
tab_file_name <- 'bt_diameter.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_diameter)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)


#--------------------------------------------------
# Transformation of Density
#--------------------------------------------------


bt_density <- boxTidwell(formula =
                           log_Price ~
                           # Width +
                           # Diameter +
                           Density,
                         other.x = ~
                           Width +
                           Diameter +
                           # Density +
                           Sealed +
                           Machined +
                           made_in_USA +
                           made_in_USA*Sealed,
                         max.iter = 100,
                         tol = 0.001,
                         data = flyreels)

print(bt_density)
# MLE of lambda Score Statistic (z) Pr(>|z|)
#       0.19315             -1.3448   0.1787
#
# iterations =  10

# Similar to Diameter, this is very weak evidence for
# a square root transformation
# for diameter but it is not estimated accurately.

# Print the output to a LaTeX file.
tab_file_name <- 'bt_density.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_density)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)



# Conclude that the linear model is the best choice.




##################################################
# End
##################################################
