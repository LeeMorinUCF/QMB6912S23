##################################################
#
# QMB 6912 Capstone Project
# PMSM-BA program
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# January 9, 2023
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
# wd_path <- '~/GitHub/QMB6912S23/demo_02/FlyReel_Density'
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

# Smoothing package for other ways of creating density plots.
library(sm)

# No other libraries required.
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
fig_file_name <- 'ecdf_prices.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(ecdf_price,
     main = 'Empirical Cumulative Distribution Function of Fly Reel Prices',
     xlab = 'Price',
     ylab = 'Empirical C.D.F.')
dev.off()


##################################################
# Relative histogram of price.
print('Plotting relative histogram of price.')
##################################################

fig_file_name <- 'hist_prices.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
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
fig_file_name <- 'density_prices.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(density_log_price,
     main = 'Kernel-smoothed pdf of the Natural Log. of Fly Reel Prices',
     xlab = 'Price')
dev.off()



##################################################
# Relative histogram and density of Price.
print('Plotting figures by Country of Manufacture.')
##################################################

# Investigate the value of fly reels made in America.
table(flyreels[, 'Country'], useNA = 'ifany')


# The densities could be plotted separately.
plot(density(flyreels[flyreels[, 'Country'] == 'USA', 'Price']),
     col = 'blue',
     lwd = 3,
     xlim = c(-100, 1300),
     ylim = c(0, 0.003)
     )
lines(density(flyreels[flyreels[, 'Country'] == 'China', 'Price']),
      col = 'red',
      lwd = 3)
lines(density(flyreels[flyreels[, 'Country'] == 'Korea', 'Price']),
      col = 'darkgreen',
      lwd = 3)
# This approach is useful because it clearly specifies
# which is which.


# Now plot them together with the sm package.
# It appears as though it plots them in order of the values
# of the Country indicator.
fig_file_name <- 'dens_by_country.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
sm.density.compare(log(flyreels[, 'Price']),
                   flyreels[, 'Country'],
                   xlab = "Log. of Price",
                   lwd = 3,
                   col = c('red','darkgreen', 'blue'))
title(main = 'Log. of Price by Country of Manufacture')
legend('topright', c('USA', 'China', 'Korea'),
       fill = c('blue', 'red','darkgreen'),
       cex = 0.75)
dev.off()






##################################################
# End
##################################################
