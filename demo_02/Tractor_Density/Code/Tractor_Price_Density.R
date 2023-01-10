##################################################
#
# QMB 6912 Capstone Project in Business Analytics
#
# Analyzing the Dependent Variable
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
# Tractor_Price_Density analyzes the prices of used tractors,
# the dependent variable in the TRACTOR7.csv dataset.
# The output includes plots of the histogram
# and kernel-smoothed density plots.
#
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))

##################################################
# Preparing the Workspace
##################################################


# Set working directory, if running interactively.
# wd_path <- '~/GitHub/QMB6912S23/demo_02/Tractor_Density'
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

in_file_name <- sprintf('%s/%s', data_dir, 'TRACTOR7.csv')
tractor_sales <- read.csv(file = in_file_name)

# Inspect the contents.
print('Summary of tractor_sales Dataset:')
print(summary(tractor_sales))

# Make sure there are no problems with the data.



##################################################
# Relative histogram of saleprice.
print('Plotting relative histograms of saleprice.')
##################################################

# First plot a histogram with the default options.
fig_file_name <- 'hist_saleprice.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
hist(tractor_sales[, 'saleprice'],
     main = 'Relative Histogram of Tractor Prices',
     xlab = 'Price',
     probability = TRUE)
dev.off()
# Notice that there are some very large values.
# Consider taking logs to bring outliers closer to the others.

# Generate a new variable log_saleprice.
tractor_sales[, 'log_saleprice'] <- log(tractor_sales[, 'saleprice'])

# Now plot the histogram for log of saleprice:
fig_file_name <- 'hist_log_saleprice.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
hist(tractor_sales[, 'log_saleprice'],
     main = 'Histogram of the Logarithm of Tractor Prices',
     xlab = 'Logarithm of Price',
     probability = TRUE)
dev.off()
# This is much better behaved. It looks almost normal.

# Adjust the tuning parameter for the number of
# bars in the chart for the histogram.

# A low number of breaks may give a smoother plot but
# it may not be very informative.
fig_file_name <- 'hist_log_saleprice_br5.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
hist(tractor_sales[, 'log_saleprice'], breaks = 5,
     main = c('Histogram of the Logarithm of Tractor Prices',
              'Number of Breaks: 5'),
     xlab = 'Logarithm of Price',
     probability = TRUE)
dev.off()

# On the other extreme,
# a high number of breaks gives a sparsely populated
# and jagged plot.
fig_file_name <- 'hist_log_saleprice_br50.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
hist(tractor_sales[, 'log_saleprice'], breaks = 50,
     main = c('Histogram of the Logarithm of Tractor Prices',
              'Number of Breaks: 50'),
     xlab = 'Logarithm of Price',
     probability = TRUE)
dev.off()

# In the limit, the bars can be very small
# to the point that each bar is a pixel wide:
# approximating a continuous function.
# To smooth it out, the nonparametric technique of
# kernel smoothing with produce a continuous function.

##################################################
# Univariate kernel estimation
print('Plotting kernel-smoothed densities of saleprice.')
##################################################

# Kernel-density smoothing is an example of a nonparametric method.
# You may have used nonparametric methods to plot a density.

price_density <- density(tractor_sales[, 'saleprice'])


fig_file_name <- 'density_saleprice.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(price_density,
     main = 'Kernel-Smoothed Density of Tractor Prices',
     xlab = 'Price')
dev.off()

# In the default, the bandwidth is chosen using an algorithm.
# See the help for density.
attributes(price_density)
price_density$bw

# But you can choose it as a tuning parameter.
price_density <- density(tractor_sales[, 'saleprice'],
                         bw = 10000)


fig_file_name <- 'density_saleprice_bw10000.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(price_density,
     main = c('Kernel-Smoothed Density of Tractor Prices',
              'Bandwidth: 10,000'),
     xlab = 'Price')
dev.off()

# A bigger bandwidth gives you a smooth density,
# but might smooth over the details.


# A smaller bandwidth might make the density too noisy.
price_density <- density(tractor_sales[, 'saleprice'],
                         bw = 1000)


fig_file_name <- 'density_saleprice_bw1000.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(price_density,
     main = c('Kernel-Smoothed Density of Tractor Prices',
              'Bandwidth: 1,000'),
     xlab = 'Price')
dev.off()

# There are many jagged changes that have more to do
# with the particular values observed
# than with the population density.

# We can do something similar to predict one variable
# with the others.
# Before that, we will transform the dependent variable,
# after analyzing it in greater detail.

# For now, we can plot the logarithm of the dependent variable.
log_price_density <- density(tractor_sales[, 'log_saleprice'],
                         bw = 0.20)


fig_file_name <- 'density_log_saleprice_bw020.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(log_price_density,
     main = c('Density of the Logarithm of Tractor Prices',
              'Bandwidth: 0.20'),
     xlab = 'Logarithm of Price')
dev.off()

# This is much better behaved and is a good place to start.




##################################################
# Relative histogram and density of saleprice.
print('Plotting figures by Brand.')
##################################################

# Investigate the value of John Deere tractors.


# The densities could be plotted separately.
plot(density(tractor_sales[tractor_sales[, 'johndeere'] == 1, 'log_saleprice']),
     col = 'green',
     lwd = 3)
lines(density(tractor_sales[tractor_sales[, 'johndeere'] == 0, 'log_saleprice']),
      col = 'red',
      lwd = 3)
# This helps me verify which is which.


# Now plot them both with the sm package.
# It appears as though it plots them in order of the values
# of the John Deere indicator.
fig_file_name <- 'dens_by_brand.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
sm.density.compare(tractor_sales[, 'log_saleprice'],
                   tractor_sales[, 'johndeere'],
                   xlab = "Log. of Sale Price",
                   lwd = 3,
                   col = c('red','green'))
title(main = 'Log. of Sale Price by Brand')
legend('topright', c('Other', 'John Deere'),
       fill = c('red','green'),
       cex = 0.75)
dev.off()




##################################################
# End
##################################################
