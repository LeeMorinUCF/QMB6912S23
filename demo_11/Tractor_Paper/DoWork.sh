#!/bin/bash

################################################################################
#
# QMB 6912: Capstone in Business Analytics
# Shell Script for Sample Document
#
# Name:
# College of Business
# University of Central Florida
#
# Date:
#
################################################################################
#
# This shell script builds a document with LaTeX.
#
# Note: The top line tells where your bash program is located
#     and should match the result you get when you
#     type the command "which bash".
#     To run this script you have to navigate to this folder in
#     a terminal window, such as GitBash, and execute
#     ./my_shell_script.sh
#     where the name of the .sh file corresponds to the name of this file.
#
################################################################################


################################################################################
# Generate the Tables with R
################################################################################

echo "#-------------------------------------------------"
echo ""
echo "Analyzing the data in R..."
echo ""

# Rscript Code/Tractor_Tables.R > Code/Tractor_Tables.out
Rscript Code/Tractor_Price_Density.R > Code/Tractor_Price_Density.out
Rscript Code/Tractor_Data_Vis.R > Code/Tractor_Data_Vis.out
Rscript Code/Tractor_Box_Cox.R > Code/Tractor_Box_Cox.out
Rscript Code/Tractor_Reg_Models.R > Code/Tractor_Reg_Models.out
Rscript Code/Tractor_Nonparametric.R > Code/Tractor_Nonparametric.out
Rscript Code/Tractor_Box_Tidwell.R > Code/Tractor_Box_Tidwell.out
Rscript Code/Tractor_SampleSelection.R > Code/Tractor_SampleSelection.out

echo "#-------------------------------------------------"
echo ""
echo "Finished analyzing the data in R."
echo ""

################################################################################
# Build the pdf Document with LaTeX
################################################################################

echo "#-------------------------------------------------"
echo ""
echo "Building the pdf document with LaTeX..."
echo ""

# The default version needs no options.
# pdflatex name_of_my_paper.tex

cd Paper

# We need options for extra permission in the VirtualBox machine.
pdflatex -shell-escape Tractor_Paper.tex

# Run the command twice to obtain references in document.
pdflatex -shell-escape Tractor_Paper.tex

echo ""

echo "Finished building the pdf document with LaTeX."
echo ""
echo "#-------------------------------------------------"
echo ""


################################################################################
# End
################################################################################
