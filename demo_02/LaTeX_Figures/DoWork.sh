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
# Generate the Figures with R
################################################################################

echo "#-------------------------------------------------"
echo ""
echo "Analyzing the data in R..."
echo ""

Rscript Code/my_script.R > Code/my_script.out

echo "#-------------------------------------------------"
echo ""
echo "Finished analyzing the data in R."
echo ""


################################################################################
# Build the pdf Document with LaTeX
################################################################################

echo "#-------------------------------------------------"
echo ""
echo "Building the pdf Document with LaTeX..."
echo ""

cd Paper
# The default version needs no options.
# pdflatex Paper.tex

# Need extra permission in the VirtualBox machine.
pdflatex -shell-escape Paper_w_Figure.tex

# Run the command twice to obtain references in document.
pdflatex -shell-escape Paper_w_Figure.tex

cd ..
echo ""

echo "Finished building the pdf document with LaTeX."
echo ""
echo "#-------------------------------------------------"
echo ""


################################################################################
# End
################################################################################
