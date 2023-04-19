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

Rscript Code/FlyReel_Price_Density.R > Code/FlyReel_Price_Density.out
Rscript Code/FlyReel_Box_Cox.R > Code/FlyReel_Box_Cox.out
Rscript Code/FlyReel_Tables.R > Code/FlyReel_Tables.out
Rscript Code/FlyReel_Data_Vis.R > Code/FlyReel_Data_Vis.out
Rscript Code/FlyReel_Reg_Models.R > Code/FlyReel_Reg_Models.out
Rscript Code/FlyReel_Box_Tidwell.R > Code/FlyReel_Box_Tidwell.out
Rscript Code/FlyReel_Nonparametric.R > Code/FlyReel_Nonparametric.out
Rscript Code/FlyReel_SampleSelection.R > Code/FlyReel_SampleSelection.out

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
pdflatex -shell-escape FlyReel_Paper.tex

# Run the command twice to obtain references in document.
pdflatex -shell-escape FlyReel_Paper.tex

echo ""

echo "Finished building the pdf document with LaTeX."
echo ""
echo "#-------------------------------------------------"
echo ""


################################################################################
# End
################################################################################
