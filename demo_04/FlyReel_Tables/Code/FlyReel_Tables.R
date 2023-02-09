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
# January 26, 2022
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
# wd_path <- '~/GitHub/QMB6912S23/demo_04/FlyReel_Tables'
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

# Set parameters for flyreel dataset.
in_file_name <- sprintf('%s/%s', data_dir, 'FlyReels.csv')
fly_col_names <- c('Name', 'Brand', 'Weight', 'Diameter', 'Width',
                   'Price', 'Sealed', 'Country', 'Machined')

# Load data.
flyreels <- read.csv(file = in_file_name, header = FALSE,
                     col.names = fly_col_names)

# Initial inspection.
print("Summary of FlyReels.csv dataset:")
print(summary(flyreels))

##################################################
# Summarize numeric variables.
##################################################

#--------------------------------------------------
print('Summarizing Numeric Variables')

print('Summary by Country of Manufacture:')
#--------------------------------------------------

# Summarize numeric variables by country of manufacture.
country_sum <- data.frame(Country = unique(flyreels$Country))
for (var_name in colnames(flyreels)[lapply(flyreels, class) == 'numeric']) {

  col_names <- sprintf('%s %s', c('Min.', 'Mean', 'Max.'), var_name)
  # country_sum[, col_names] <- tapply(flyreels$Price, flyreels$Country,
  #                                    function(x) format(summary(x), scientific = FALSE)[c(1,4,6)])
  # country_sum[, col_names] <- tapply(flyreels[, var_name], flyreels$Country,
  #                                    function(x) format(summary(x), scientific = FALSE)[c(1,4,6)])
  sub_tab <- tapply(flyreels[, var_name], flyreels$Country,
                    function(x) format(summary(x), scientific = FALSE)[c(1,4,6)])
  for (row in 1:length(sub_tab)) {
    country_sum[row, col_names] <- unlist(sub_tab[row])
  }
  
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
                     digits = 0, label = 'tab:summ_by_country',
                     caption = 'Summary by Country of Manufacture')

tab_file_name <- sprintf('summ_by_country.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)





##################################################
# Summarize categorical variables.
##################################################

#--------------------------------------------------
print('Summarizing Categorical Variables')
#--------------------------------------------------


# Inspect visually before creating tables.
table(flyreels[, 'Brand'], useNA = 'ifany')
table(flyreels[, 'Sealed'], useNA = 'ifany')
table(flyreels[, 'Country'], useNA = 'ifany')
table(flyreels[, 'Machined'], useNA = 'ifany')

# Comparison across brand names.
table(flyreels[, 'Brand'], flyreels[, 'Sealed'], useNA = 'ifany')
table(flyreels[, 'Brand'], flyreels[, 'Country'], useNA = 'ifany')
table(flyreels[, 'Brand'], flyreels[, 'Machined'], useNA = 'ifany')


#--------------------------------------------------
print('Country of Manufacture by Brand of Fly Reel')
#--------------------------------------------------


# Assemble these into a table for output.
out_tab <- cbind(table(flyreels[, 'Brand'], useNA = 'ifany'),
                 table(flyreels[, 'Brand'], flyreels[, 'Country'], useNA = 'ifany'),
                 table(flyreels[, 'Brand'], flyreels[, 'Sealed'], useNA = 'ifany'),
                 table(flyreels[, 'Brand'], flyreels[, 'Machined'], useNA = 'ifany')
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
                     digits = 0, label = 'tab:country_by_brand',
                     caption = 'Country of Manufacture by Brand of Fly Reel')

tab_file_name <- sprintf('country_by_brand.tex')
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
# Analyze Correlation.
##################################################

corr_var_list <- c('Price', 'Weight', 'Diameter', 'Width')

# Calculate correlation matrix.
out_tab <- cor(flyreels[, corr_var_list])
colnames(out_tab) <- corr_var_list
rownames(out_tab) <- corr_var_list
print(out_tab)


out_xtable <- xtable(out_tab[, ],
                     digits = 3,
                     label = 'tab:correlation_num',
                     caption = 'Correlation Matrix of Prices and Numeric Variables')

tab_file_name <- sprintf('correlation_num.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)



##################################################
# End
##################################################
