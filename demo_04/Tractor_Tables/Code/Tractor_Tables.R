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
# This script summarizes the data
# by creating tables for rendering in LaTeX.
#
# Dependencies:
#   xtable for creating code for LaTeX tables
#
#
##################################################


##################################################
# Preparing the Workspace
##################################################


# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory, if running interactively.
# wd_path <- '~/GitHub/QMB6912S23/demo_04/Tractor_Tables'
# setwd(wd_path)


# Set data directory.
data_dir <- 'Data'

# Set directory for storing figures.
# fig_dir <- 'Figures' # Last week.

# Set directory for storing tables.
tab_dir <- 'Tables'


##################################################
# Load libraries
##################################################

# install.packages('xtable')
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
##################################################
##################################################
# Change code to analyze tractor prices.
##################################################
##################################################
##################################################










##################################################
# Summarize numeric variables.
##################################################

#--------------------------------------------------
print('Summarizing Numeric Variables')

print('Summary by make:')
#--------------------------------------------------

# Summarize numeric variables by make: John Deere or other.
country_sum <- data.frame(Country = unique(tractor_sales$Country))
for (var_name in colnames(flyreels)[lapply(flyreels, class) == 'numeric']) {

  col_names <- sprintf('%s %s', c('Min.', 'Mean', 'Max.'), var_name)
  # country_sum[, col_names] <- tapply(tractor_sales$Price, tractor_sales$Country,
  #                                    function(x) format(summary(x), scientific = FALSE)[c(1,4,6)])
  country_sum[, col_names] <- tapply(tractor_sales[, var_name], tractor_sales$Country,
                                     function(x) format(summary(x), scientific = FALSE)[c(1,4,6)])

}

# Select values for output.
# t(X) denotes the transpose of X.
out_tab <- t(country_sum[, 2:ncol(country_sum)])
colnames(out_tab) <- country_sum[, 1]
print(out_tab)

#--------------------------------------------------
# Output to TeX file.
#--------------------------------------------------

out_xtable <- xtable(out_tab[, ],
                     digits = 0, label = 'tab:summ_by_make',
                     caption = 'Summary by Make of Tractor')

tab_file_name <- sprintf('summ_by_make.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)





##################################################
# Summarize categorical variables.
##################################################

#--------------------------------------------------
print('Summarizing Categorical Variables')
#--------------------------------------------------


# Inspect visually before creating tables.
table(tractor_sales[, 'Brand'], useNA = 'ifany')
table(tractor_sales[, 'Sealed'], useNA = 'ifany')
table(tractor_sales[, 'Country'], useNA = 'ifany')
table(tractor_sales[, 'Machined'], useNA = 'ifany')

# Comparison across brand names.
table(tractor_sales[, 'Brand'], tractor_sales[, 'Sealed'], useNA = 'ifany')
table(tractor_sales[, 'Brand'], tractor_sales[, 'Country'], useNA = 'ifany')
table(tractor_sales[, 'Brand'], tractor_sales[, 'Machined'], useNA = 'ifany')


#--------------------------------------------------
print('Make of Tractor by ...')
#--------------------------------------------------


# Assemble these into a table for output.
out_tab <- cbind(table(tractor_sales[, 'Brand'], useNA = 'ifany'),
                 table(tractor_sales[, 'Brand'], tractor_sales[, 'Country'], useNA = 'ifany'),
                 table(tractor_sales[, 'Brand'], tractor_sales[, 'Sealed'], useNA = 'ifany'),
                 table(tractor_sales[, 'Brand'], tractor_sales[, 'Machined'], useNA = 'ifany')
)

# Specify column names and add totals.
colnames(out_tab) <- c("Total", "China", "Korea", "USA",
                       "Unsealed", "Sealed", "Cast", "Machined")
out_tab <- rbind(out_tab, colSums(out_tab))
rownames(out_tab)[length(rownames(out_tab))] <- "Totals"
print(out_tab)


#--------------------------------------------------
# Output selected columns to TeX file.
#--------------------------------------------------

out_xtable <- xtable(out_tab[, c(2, 3, 4, 1)],
                     digits = 0, label = 'tab:make_by_',
                     caption = 'Country of Manufacture by Brand of Fly Reel')

tab_file_name <- sprintf('make_by.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)


#--------------------------------------------------
print('Reel Design by Brand of Fly Reel')
#--------------------------------------------------


# Output another set of columns to another TeX file.
out_xtable <- xtable(out_tab[, c(5:8, 1)],
                     digits = 0, label = 'tab:design_by_brand',
                     caption = 'Reel Design by Brand of Fly Reel')

tab_file_name <- sprintf('design_by_brand.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)
















##################################################
# End
##################################################

