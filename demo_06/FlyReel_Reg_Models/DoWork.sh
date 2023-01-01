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

Rscript Code/FlyReel_Reg_Models.R > Code/FlyReel_Reg_Models.out

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
pdflatex -shell-escape FlyReel_Reg_Models.tex

# Run the command twice to obtain references in document.
pdflatex -shell-escape FlyReel_Reg_Models.tex

echo ""

echo "Finished building the pdf document with LaTeX."
echo ""
echo "#-------------------------------------------------"
echo ""


################################################################################
# End
################################################################################
