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
# February 15, 2022
#
##################################################
#
# Tractor_Data_Vis analyzes the prices of used tractors,
# the dependent variable in the TRACTOR7.csv dataset.
# The output includes plots of scatterplot matrices
# and other data visualizations.
#
# Dependencies:
#   sm for smoothing density estimates.
#   vcd for Visualizing Categorical Data
#   gclus for color-coding by sign of correlation.
#     using cluster analysis.
#   xtable for creating code for LaTeX tables
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
# wd_path <- '~/GitHub/QMB6912S23/demo_05/Tractor_Data_Vis'
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

# Library for smoothing density estimates.
library(sm)

# Library for Visualizing Categorical Data
library(vcd)

# Library for Scatter plot matrix with color-coding
# by sign of correlation.
library(gclus)


# Library for creating code for LaTeX tables.
library(xtable)

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
# Data Preparation
##################################################


# Generate a new variable log_saleprice.
tractor_sales[, 'log_saleprice'] <- log(tractor_sales[, 'saleprice'])


##################################################
# Relative histogram and density of saleprice.
print('Plotting histogram and density of log saleprice.')
##################################################

# Start with the log of sale prices because that
# seemed more promising, given that prices were highly skewed.

# First plot a histogram with the default options.
fig_file_name <- 'hist_dens_log_price.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# For an eps file, as before:
# setEPS()
# postscript(out_file_name)
# This produces a pdf instead:
pdf(out_file_name)
hist(tractor_sales[, 'log_saleprice'],
     main = 'Histogram and Density of Log. Tractor Prices',
     xlab = 'Price',
     col = 'red',
     probability = TRUE)
rug(tractor_sales[, 'log_saleprice'])
lines(density(tractor_sales[, 'log_saleprice']),
      col = 'blue',
      lwd = 3)
dev.off()

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
# Relative histogram and density of saleprice.
print('Plotting figures by Season of Sale')
##################################################

# Investigate the price of tractors during different seasons..


# Again, the densities could be plotted separately.

# Create an indicator for fall (the default season).
tractor_sales[, 'fall'] <- tractor_sales[, 'spring'] == 0 &
        tractor_sales[, 'summer'] == 0 &
        tractor_sales[, 'winter'] == 0

# Plot several densities together by season.
fig_file_name <- 'dens_by_season.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(density(tractor_sales[tractor_sales[, 'fall'] == 1, 'log_saleprice']),
     col = 'orange',
     lwd = 3,
     xlim = c(min(tractor_sales[, 'log_saleprice']),
              max(tractor_sales[, 'log_saleprice'])),
     main = 'Log. of Sale Price by Season of Sale')
# Plot the rest.
lines(density(tractor_sales[tractor_sales[, 'spring'] == 1, 'log_saleprice']),
     col = 'green',
     lwd = 3)
lines(density(tractor_sales[tractor_sales[, 'summer'] == 1, 'log_saleprice']),
      col = 'yellow',
      lwd = 3)
lines(density(tractor_sales[tractor_sales[, 'winter'] == 1, 'log_saleprice']),
      col = 'blue',
      lwd = 3)
legend('topright',
       c('Spring', 'Summer', 'Fall', 'Winter'),
       fill = c('green', 'yellow', 'orange', 'blue'),
       cex = 0.65)
dev.off()






# We see that the distribution of sales is similar
# during summer and fall (yellow and orange, respectively)
# with some bunching in the upper tail.
# We also see more variance in the sale price in winter,
# shown in blue.
# In the spring, shown in green, the mode of the distribution
# is higher.



##################################################
# Investigate whether Brand is Related to Time of Year
print('Plotting Sales Volume by Brand and Season of Sale')
##################################################

# Create a factor that is a single categorical variable season.
tractor_sales[, 'season'] <- NA
tractor_sales[tractor_sales[, 'spring'] == 1, 'season'] <- 'Spring'
tractor_sales[tractor_sales[, 'summer'] == 1, 'season'] <- 'Summer'
tractor_sales[tractor_sales[, 'winter'] == 1, 'season'] <- 'Winter'
tractor_sales[tractor_sales[, 'fall'] == 1, 'season'] <- 'Fall'
tractor_sales[, 'season'] <- factor(tractor_sales[, 'season'])

# Create a factor for the John Deere indicator.
tractor_sales[, 'JD'] <- NA
tractor_sales[tractor_sales[, 'johndeere'] == 1, 'JD'] <- 'John Deere'
tractor_sales[tractor_sales[, 'johndeere'] == 0, 'JD'] <- 'Other'
tractor_sales[, 'JD'] <- factor(tractor_sales[, 'JD'])


# Create a table and plot it in a spinogram.
counts <- table(tractor_sales[, 'season'],
                tractor_sales[, 'JD'])

# Plot the spinogram.
fig_file_name <- 'brand_and_season_sales.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
spine(counts,
      main = 'Spinogram of Sales by Brand and Season')
dev.off()

#--------------------------------------------------
# Output Sales Volume Table to TeX file.
#--------------------------------------------------


# Select values for output.
out_tab <- counts
print(out_tab)


out_xtable <- xtable(out_tab[, ],
                     digits = 0,
                     label = 'tab:brand_and_season_sales',
                     caption = 'Sales Volume by Brand and Season')

tab_file_name <- sprintf('brand_and_season_sales.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)



##################################################
# Investigate correlation of numerical variables
# Scatter plot matrix
# Color-coding produced with the gclus package
print('Plotting Prices agains Numeric Variables')
##################################################

# Select some numerical variables.
colnames(tractor_sales)
colnames(tractor_sales)[c(13, 2, 3, 4)]

# Create a covariance matrix and determine
# parameters for scattergraph matrix.
mydata <- tractor_sales[c(13, 2, 3, 4)]
mydata.corr <- abs(cor(mydata))
mycolors <- dmat.color(mydata.corr)
# Order by magnitude of correlation.
myorder <- order.single(mydata.corr)
# myorder <- c(1,2,3,4) # Retain order.


# Plot the scatterplot matrix.
fig_file_name <- 'scatter_matrix.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
cpairs(mydata,
       myorder,
       panel.colors = mycolors,
       gap = 0.5,
       main = c('Scatterplot Matrix Colored by Correlation')
)
dev.off()


#--------------------------------------------------
# Output Correlation Matrix to TeX file.
#--------------------------------------------------


# Select values for output.
out_tab <- cor(mydata)
colnames(out_tab) <- c('Log. of Price', 'Horsepower', 'Age', 'Engine Hours')
rownames(out_tab) <- c('Log. of Price', 'Horsepower', 'Age', 'Engine Hours')
print(out_tab)


out_xtable <- xtable(out_tab[, ],
                     digits = 3,
                     label = 'tab:correlation',
                     caption = 'Correlation Matrix of Numeric Variables')

tab_file_name <- sprintf('correlation.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)



##################################################
# Investigate relationship between prices,
# horsepower and age.
print('Plotting relationship between prices, horsepower and age')
##################################################

# Calculate the radius of the bubbles
# so that the area represents horsepowwer.
r <- sqrt(tractor_sales[, 'horsepower']/pi)

# Plot the bubble plot.
fig_file_name <- 'bubble_plot.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
symbols(tractor_sales[, 'age'],
        tractor_sales[, 'log_saleprice'],
        r,
        inches=0.30, fg="white", bg="lightblue",
        main = "Bubble Plot with point size proportional to horsepower",
        ylab = "Log. of Sale Price",
        xlab = "Age of Tractor (years)")
dev.off()



##################################################
# End
##################################################
