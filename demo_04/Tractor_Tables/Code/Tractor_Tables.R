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
# wd_path <- '~/Teaching/UCF_BA_Capstones/QMB6912_Spring_2023/GitHub/QMB6912S23/demo_04/Tractor_Tables'
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


# Inspect the types of variables.
lapply(tractor_sales, class)
# All are integer but many are logical or numeric but stored as integers.





##################################################
# Summarize numeric variables.
##################################################

#--------------------------------------------------
print('Summarizing Numeric Variables')

print('Summary by make:')
#--------------------------------------------------

# Summarize numeric variables by make: John Deere or other.
johndeere_sum <- data.frame(johndeere = unique(tractor_sales$johndeere))

# Select the list of numeric variables to analyze.
# Inspect the types of variables.
lapply(tractor_sales, class)
# All are integer but many are logical or numeric but stored as integers.

# For the purpose of tabulating data, we can select whatever variables are of interest.
tab_var_list <- colnames(tractor_sales)[c(1:4)]
# All numeric variables, excluding indicators and the seasons, which we will investigate separately.


for (var_name in tab_var_list) {

  col_names <- sprintf('%s %s', c('Min.', 'Mean', 'Max.'), var_name)

  # Calculate the selected statistics.
  tab_stats <- tapply(tractor_sales[, var_name], tractor_sales$johndeere,
                      function(x) format(summary(x), scientific = FALSE)[c(1,4,6)])

  # Assign them to the rows of the table.
  for (rownum in 1:nrow(johndeere_sum)) {
    johndeere_sum[rownum, col_names] <- tab_stats[[rownum]]
  }

}

# Select values for output.
# t(X) denotes the transpose of X.
# This exchanges the rows and columns.
out_tab <- t(johndeere_sum[, 2:ncol(johndeere_sum)])
colnames(out_tab) <- c('Other', 'John Deere')
# Switch column order to focus on John Deere.
out_tab <- out_tab[, c(2, 1)]
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

tab_var_list <- colnames(tractor_sales)[c(5:7, 9)]

# Inspect visually before creating tables.
table(tractor_sales[, 'johndeere'], useNA = 'ifany')
table(tractor_sales[, 'diesel'], useNA = 'ifany')
table(tractor_sales[, 'fwd'], useNA = 'ifany')
table(tractor_sales[, 'manual'], useNA = 'ifany')
table(tractor_sales[, 'cab'], useNA = 'ifany')

# Comparison across brand names.
table(tractor_sales[, 'johndeere'], tractor_sales[, 'diesel'], useNA = 'ifany')
table(tractor_sales[, 'johndeere'], tractor_sales[, 'fwd'], useNA = 'ifany')
table(tractor_sales[, 'johndeere'], tractor_sales[, 'manual'], useNA = 'ifany')
table(tractor_sales[, 'johndeere'], tractor_sales[, 'cab'], useNA = 'ifany')


#--------------------------------------------------
print('Indicator Variables by Make of Tractor')
#--------------------------------------------------


# Assemble these into a table for output.
out_tab <- rbind(table(tractor_sales[, 'johndeere'], useNA = 'ifany'),
                 table(tractor_sales[, 'diesel'], tractor_sales[, 'johndeere'], useNA = 'ifany'),
                 table(tractor_sales[, 'fwd'], tractor_sales[, 'johndeere'], useNA = 'ifany'),
                 table(tractor_sales[, 'manual'], tractor_sales[, 'johndeere'], useNA = 'ifany'),
                 table(tractor_sales[, 'cab'], tractor_sales[, 'johndeere'], useNA = 'ifany')
)

# Specify column and row names and add totals.
colnames(out_tab) <- c("Other", "John Deere")
rownames(out_tab) <- c("Total",
                       "Gasoline", "Diesel",
                       "2WD", "4WD",
                       "Automatic", "Manual",
                       "No Cab", "Has Cab")
# Switch column order to focus on John Deere.
out_tab <- out_tab[, c(2, 1)]
# Add totals for rows.
out_tab <- cbind(out_tab, rowSums(out_tab))
colnames(out_tab)[length(colnames(out_tab))] <- "Totals"
print(out_tab)


#--------------------------------------------------
# Output selected columns to TeX file.
#--------------------------------------------------

out_xtable <- xtable(out_tab[, ],
                     digits = 0, label = 'tab:ind_by_make',
                     caption = 'Indicator Variables by Make of Tractor')

tab_file_name <- sprintf('ind_by_make.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)


#--------------------------------------------------
print('Season Sold by Make of Tractor')
#--------------------------------------------------

# Indicators for season sold represent a single categorical variable.
# Better to analyze these together.

# First, create a single categorical variable for the seasons.
tractor_sales[, 'season_sold'] <- 'Fall'
tractor_sales[tractor_sales[, 'spring'] == 1, 'season_sold'] <- 'Spring'
tractor_sales[tractor_sales[, 'summer'] == 1, 'season_sold'] <- 'Summer'
tractor_sales[tractor_sales[, 'winter'] == 1, 'season_sold'] <- 'Winter'

table(tractor_sales[, 'season_sold'], useNA = 'ifany')
table(tractor_sales[, 'season_sold'], tractor_sales[, 'johndeere'], useNA = 'ifany')

out_tab <- table(tractor_sales[, 'season_sold'], tractor_sales[, 'johndeere'], useNA = 'ifany')

colnames(out_tab) <- c("Other", "John Deere")
# Switch column order to focus on John Deere.
out_tab <- out_tab[, c(2, 1)]
# Add totals for rows.
out_tab <- cbind(out_tab, rowSums(out_tab))
colnames(out_tab)[length(colnames(out_tab))] <- "Totals"
# Add totals for columns.
out_tab <- rbind(out_tab, colSums(out_tab))
rownames(out_tab)[length(rownames(out_tab))] <- "Totals"
print(out_tab)




# Output another set of columns to another TeX file.
out_xtable <- xtable(out_tab[, ],
                     digits = 0, label = 'tab:season_sold_by_make',
                     caption = 'Season Sold by Make of Tractor')

tab_file_name <- sprintf('season_sold_by_make.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)




##################################################
# Summarize average prices across categorical variables.
##################################################


#--------------------------------------------------
print('Season Sold by Make of Tractor')
#--------------------------------------------------


avg_price_seasons_1 <- aggregate(x = tractor_sales[, 'saleprice'], 
                               # data = tractor_sales, 
                               FUN = mean, 
                               by = list(Season = tractor_sales[, 'season_sold'], 
                                         Brand = tractor_sales[, 'johndeere']))
print(avg_price_seasons_1)



avg_price_seasons_2 <- aggregate(x = saleprice ~ season_sold + johndeere, 
                                 data = tractor_sales,
                                 FUN = mean)


out_tab <- cbind(avg_price_seasons_2[avg_price_seasons_2[, 'johndeere'] == 1, 'saleprice'], 
                 avg_price_seasons_2[avg_price_seasons_2[, 'johndeere'] == 0, 'saleprice'])

# out_tab is a matrix.
class(out_tab)
# Change to a data.frame.
out_tab <- data.frame(out_tab)


rownames(out_tab) <- avg_price_seasons_2[1:4, 'season_sold']
colnames(out_tab) <- c('John Deere', 'Other')



# Output another set of columns to another TeX file.
out_xtable <- xtable(out_tab[, ],
                     digits = 0, label = 'tab:avg_price_by_season_sold',
                     caption = 'Average Price of Tractors by Season Sold')

tab_file_name <- sprintf('avg_price_by_season_sold.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)




##################################################
# Analyze Correlation.
##################################################


#--------------------------------------------------
print('Correlation Matrix for Numeric Variables')
#--------------------------------------------------

tab_var_list <- colnames(tractor_sales)[c(1:7, 9)]
# All numeric variables, including indicators but not seasons, which we investigated separately.

# Investigate relationship between prices, horsepower,
# age and engine hours.

# Use the log of prices, which had a better-behaved distribution.
tractor_sales[, 'log_saleprice'] <- log(tractor_sales[, 'saleprice'])
tab_var_list <- colnames(tractor_sales)[c(14, 2:4)]

# Calculate covariance matrix.
out_tab <- cor(tractor_sales[, tab_var_list])
colnames(out_tab) <- c('Log. of Price', 'Horsepower', 'Age', 'Engine Hours')
rownames(out_tab) <- c('Log. of Price', 'Horsepower', 'Age', 'Engine Hours')
print(out_tab)


out_xtable <- xtable(out_tab[, ],
                     digits = 3,
                     label = 'tab:correlation_num',
                     caption = 'Correlation Matrix of Log. Prices and Numeric Variables')

tab_file_name <- sprintf('correlation_num.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)



#--------------------------------------------------
print('Correlation Matrix for Categorical Variables')
#--------------------------------------------------


# Investigate relationship between prices and indicator variables.
tab_var_list <- colnames(tractor_sales)[c(14, 5:7,9)]

# Select values for output.
out_tab <- cor(tractor_sales[, tab_var_list])
colnames(out_tab) <- c('Log. of Price', 'Diesel', 'FWD', 'Manual', 'Cab')
rownames(out_tab) <- c('Log. of Price', 'Diesel', 'FWD', 'Manual', 'Cab')
print(out_tab)


out_xtable <- xtable(out_tab[, ],
                     digits = 3,
                     label = 'tab:correlation_ind',
                     caption = 'Correlation Matrix of Log. Prices and Indicator Variables')

tab_file_name <- sprintf('correlation_ind.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)





##################################################
# End
##################################################

