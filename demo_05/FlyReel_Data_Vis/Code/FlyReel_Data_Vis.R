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
# February 6, 2022
#
##################################################
#
# Sample code for the problem sets in the course QMB 6912,
# Capstone Project in Business Analytics, for the PMSM-BA
# program.
# This script analyzes the covariance between variables
# and makes comparisons between subsets of the data.
#
# Dependencies:
#   lattice library to create matrices of scatterplots
#
#
##################################################

##################################################
# Preparing the Workspace
##################################################


# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory, if running interactively.
# wd_path <- '~/GitHub/QMB6912S23/demo_05/FlyReel_Data_Vis'
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

# Library for smoothing density estimates.
library(sm)

# lattice library to create matrices of scatterplots
library(lattice)



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


# Relative histogram of density.
# hist(flyreels[, 'Density'],
#      main = 'Relative Histogram of Flyreel Density',
#      xlab = 'Density',
#      probability = TRUE)



# Generate a new variable log_Price.
flyreels[, 'log_Price'] <- log(flyreels[, 'Price'])




##################################################
# Relative histogram and density of Price.
print('Plotting histogram and density of log_Price.')
##################################################

# Start with the log of prices because prices were skewed.
# We will investigate this further in another problem set.

# First plot a histogram with the default options.
fig_file_name <- 'hist_dens_log_price.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# For an eps file, as before:
# setEPS()
# postscript(out_file_name)
# This produces a pdf instead:
pdf(out_file_name)
hist(flyreels[, 'log_Price'],
     main = 'Histogram and Density of Log. Fly Reel Prices',
     xlab = 'Price',
     col = 'red',
     probability = TRUE)
rug(flyreels[, 'log_Price'])
lines(density(flyreels[, 'log_Price']),
      col = 'blue',
      lwd = 3)
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
     xlim = c(0, 1200),
     ylim = c(0, 0.0025))
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
sm.density.compare(flyreels[, 'Price'],
                   flyreels[, 'Country'],
                   xlab = "Fly Reel Prices",
                   lwd = 3,
                   col = c('red','darkgreen', 'blue'))
title(main = 'Fly Reel Price by Country of Manufacture')
legend('topright', c('USA', 'China', 'Korea'),
       fill = c('blue', 'red','darkgreen'),
       cex = 0.75)
dev.off()




# Recall, however, that the log. of prices was better behaved.
# Create the same plot in logs instead.
plot(density(flyreels[flyreels[, 'Country'] == 'USA', 'log_Price']),
     col = 'blue',
     lwd = 3,
     xlim = c(2, 8))
lines(density(flyreels[flyreels[, 'Country'] == 'China', 'log_Price']),
      col = 'red',
      lwd = 3)
lines(density(flyreels[flyreels[, 'Country'] == 'Korea', 'log_Price']),
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
sm.density.compare(flyreels[, 'log_Price'],
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
# Generating Scatterplot Matrices.
print('Generating Scatterplot Matrices.')
##################################################


# Create scatterplots of numeric variables.
splom_var_list <- c('Price', 'Width', 'Diameter', 'Density')
# fig_file_name <- 'slpom_num_only.eps'
fig_file_name <- 'slpom_num_only.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# setEPS()
# postscript(out_file_name)
pdf(out_file_name)
splom(flyreels[, splom_var_list])
dev.off()


# Add some categorical variables to scatterplots.
splom_var_list <- c('Price', 'Width', 'Diameter', 'Density',
                    'Sealed', 'Machined')

# fig_file_name <- 'slpom_with_cat.eps'
fig_file_name <- 'slpom_with_cat.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# setEPS()
# postscript(out_file_name)
pdf(out_file_name)
splom(flyreels[, splom_var_list])
dev.off()
# This is a busy figure with multiple categorical variables.


# Create new categorical variable combining sealed and machined.
table(flyreels[, c('Sealed', 'Machined')], useNA = 'ifany')
flyreels[, 'Seal_Mach'] <- factor(sprintf('%s_%s',
                                   substr(flyreels[, 'Sealed'], 1, 1),
                                   substr(flyreels[, 'Machined'], 1, 1)))
table(flyreels[, c('Seal_Mach', 'Sealed')], useNA = 'ifany')
table(flyreels[, c('Seal_Mach', 'Machined')], useNA = 'ifany')

# Revise list of variables for 2-dimensional factor.
splom_var_list <- c('Price', 'Width', 'Diameter', 'Density',
                    'Seal_Mach')

# Plot in scatterplot matrix.
# fig_file_name <- 'slpom_with_sealed_mach.eps'
fig_file_name <- 'slpom_with_sealed_mach.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# setEPS()
# postscript(out_file_name)
pdf(out_file_name)
splom(flyreels[, splom_var_list])
dev.off()


##################################################
# Generating Scatterplot Matrices
# Colored by Country of Manufacture.
print(c('Generating Scatterplot Matrices',
        'Colored by Country of Manufacture.'))
##################################################


# Color by country of origin.
# fig_file_name <- 'slpom_by_country.eps'
fig_file_name <- 'slpom_by_country.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# setEPS()
# postscript(out_file_name)
pdf(out_file_name)
super.sym <- trellis.par.get("superpose.symbol")
splom(~flyreels[, splom_var_list],
      groups = Country,
      data = flyreels,
      panel = panel.superpose,
      cex = 0.5,
      varname.cex = 0.75,
      axis.text.cex = 0.1,
      axis.text.col = 'white',
      key = list(text = list(levels(flyreels[, 'Country'])),
                 title = "Three Countries of Origin",
                 columns = 3,
                 points = list(pch = super.sym$pch[1:3],
                               col = super.sym$col[1:3])
                 ))
dev.off()



##################################################
# Generating Dot Chart
# Ordered by Country of Manufacture and Brand.
print(c('Generating Dot Charts',
        'Ordered by Country of Manufacture and Brand.'))
##################################################

# Make a matrix of average prices by brand.
table(flyreels$Brand, useNA = 'ifany')
table(flyreels$Brand,
      flyreels$Country, useNA = 'ifany')


# Select the relevant columns and calculate average prices.
# Note that the function call to aggregate may depend on 
# your version of R.
# Version that works in my RStudio:
# x <- aggregate(x = Price ~ Brand + Country,
#                data = flyreels[, c('Price', 'Brand', 'Country')],
#                FUN = mean)

# Version that works in my GitBash:
x <- aggregate(formula = Price ~ Brand + Country,
               data = flyreels[, c('Price', 'Brand', 'Country')],
               FUN = mean)


# Sort the data.
x <- x[order(x$Price), ]


# Create a factor and assign color names
# by the levels of the factor.
x$color <- NA
x$color[x$Country == 'USA'] <- 'blue'
x$color[x$Country == 'China'] <- 'red'
x$color[x$Country == 'Korea'] <- 'darkgreen'

# Now plot the dotchart.
fig_file_name <- 'dotchart_brand_country.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
dotchart(x$Price,
         labels = x$Brand,
         cex = 0.7,
         pch = 19,
         groups = x$Country,
         gcolor = "black", color = x$color,
         main = "Price of Fly Reels\nGrouped by Brand",
         xlab = "Price")
dev.off()



##################################################
# End
##################################################
