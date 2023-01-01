# Generating pdf Documents with R

## Instructions:
1. Run ```DoWork.sh``` in a terminal window from the ```LaTeX_TexReg``` folder. 
This program generates a pdf document from automated data analysis. 
1. This shell script calls the ```R``` program in the ```Code``` folder, 
which creates tables, figures and text for the document, 
by analyzing a dataset read in from ```Data```. 
1. At the end of the shell script, a call to ```pdflatex``` is placed 
to build a pdf document from the ```TeX``` script in the ```Paper``` folder. 
1. The completed paper will appear as the file ```Paper.pdf``` 
within the ```Paper``` folder. 
