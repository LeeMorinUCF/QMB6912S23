##################################################
#
# QMB 6912 Capstone Project
# PMSM-BA program
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business Administration
# University of Central Florida
#
# February 3, 2022
#
##################################################
#
# Sample code for the problem sets in the course QMB 6912,
# Capstone Project in Business Analytics, for the PMSM-BA
# program.
# This script analyzes the dependent variable
# by plotting histograms and cdfs.
#
# Dependencies:
#   None
#
#
##################################################


##################################################
# Preparing the Workspace
##################################################


# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory, if running interactively.
# wd_path <- '~/GitHub/QMB6912S22/demo_04/FlyReel_Density'
# setwd(wd_path)


# Set data directory.
data_dir <- 'Data'

# Set directory for storing figures.
fig_dir <- 'Figures'

# Set directory for storing tables.
# tab_dir <- 'Tables' # Last week.


##################################################
# Load libraries
##################################################

# No libraries required.
# Otherwise would have a command like the following.
# library(name_of_R_package)



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
# Plot EDF in base R and output to figure.
print('Plotting ECDF.')
##################################################

ecdf_price <- ecdf(flyreels[, 'Price'])
fig_file_name <- 'ecdf_prices.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)
plot(ecdf_price,
     main = 'Empirical Cumulative Distribution Function of Fly Reel Prices',
     xlab = 'Price',
     ylab = 'Empirical C.D.F.')
dev.off()


##################################################
# Relative histogram of price.
print('Plotting relative histogram of price.')
##################################################

fig_file_name <- 'hist_prices.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)
hist(flyreels[, 'Price'],
     main = 'Relative Histogram of Fly Reel Prices',
     xlab = 'Price',
     probability = TRUE)
dev.off()


##################################################
# Kernel-smoothed pdf of the natural logarithm of price.
print('Plotting kernel-smoothed pdf')
print('of the natural logarithm of price.')
##################################################

density_log_price <- density(log(flyreels[, 'Price']))
fig_file_name <- 'density_prices.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)
plot(density_log_price,
     main = 'Kernel-smoothed pdf of the Natural Log. of Fly Reel Prices',
     xlab = 'Price')
dev.off()



##################################################
# End
##################################################
