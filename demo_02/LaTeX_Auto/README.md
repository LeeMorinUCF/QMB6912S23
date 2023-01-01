# Automatic Document Generation with LaTeX

One important application of File I/O is to create documents automatically.
Once a piece of analysis is complete, it is useful to hand it off to another
analyst to run periodically to produce an automated dashboard of measurements.
Furthermore, in a research project, it is often very convenient to
generate documents automatically so that they can be regenerated when the data and
the analytical procedures are adjusted.
Statistical analysis is often an iterative process; it is rare for even the best 
analysts to achieve the perfect answer on the first run. 

We will use LaTeX, 
which is a document markup language for the TeX typesetting program. 

## A Simple Example

Last week, we used a minimal document
that can be created with the following commands, 
available in the file called ```Paper_0_Simple.tex```.

```
\documentclass[11pt]{article}
\begin{document}
This is my document.
\end{document}
```

This program sets the class of the document to ```paper``` but it could also be set to 
```article```, ```book```, ```report``` or ```letter```.
The ```document``` ```begin```s on line 2 and ```end```s on line 4. 
Line 3 is the single sentence that is printed in the document. 

Documents are typically divided into ```sections``` and ```subsections```. 
Elements of a document, such as sections, tables, figures and equations are ```ref```erenced by ```label```s. 
Using programmatic references to components of the document eliminates the need to change reference numbers when the components are put in a different order or
when components are added or removed. 

You can build the pdf file from this script bu pressing "Run"
in a typesetting GUI such as TeXworks, the default editor with MiKTeX. 
This can be run automatically at the command line
as in the shell script ```Build_0_Simple.sh```, 
which has the following code, in a generic form. 

```
pdflatex name_of_my_paper.tex
```

On some platforms, additional options are required 
for extra permission, such as in the VirtualBox machine.
```
pdflatex -shell-escape Paper_0_Simple.tex
```
As with any shell script, you run this script at the command line:

```
$ ./Build_0_Simple.sh
#-------------------------------------------------

Building the pdf Document with LaTeX...

This is pdfTeX, Version 3.141592653-2.6-1.40.22 (MiKTeX 21.3)
entering extended mode
(Paper_0_Simple.tex
LaTeX2e <2020-10-01> patch level 4
L3 programming layer <2021-05-11>
(C:\Users\le279259\AppData\Local\Programs\MiKTeX\tex/latex/base\article.cls
Document Class: article 2020/04/10 v1.4m Standard LaTeX document class
(C:\Users\le279259\AppData\Local\Programs\MiKTeX\tex/latex/base\size11.clo))
(C:\Users\le279259\AppData\Local\Programs\MiKTeX\tex/latex/l3backend\l3backend-
pdftex.def)
No file Paper_0_Simple.aux.
[1{C:/Users/le279259/AppData/Local/MiKTeX/pdftex/config/pdftex.map}]
(Paper_0_Simple.aux) )<C:/Users/le279259/AppData/Local/Programs/MiKTeX/fonts/ty
pe1/public/amsfonts/cm/cmr10.pfb>
Output written on Paper_0_Simple.pdf (1 page, 13588 bytes).
Transcript written on Paper_0_Simple.log.
This is pdfTeX, Version 3.141592653-2.6-1.40.22 (MiKTeX 21.3)
entering extended mode
(Paper_0_Simple.tex
LaTeX2e <2020-10-01> patch level 4
L3 programming layer <2021-05-11>
(C:\Users\le279259\AppData\Local\Programs\MiKTeX\tex/latex/base\article.cls
Document Class: article 2020/04/10 v1.4m Standard LaTeX document class
(C:\Users\le279259\AppData\Local\Programs\MiKTeX\tex/latex/base\size11.clo))
(C:\Users\le279259\AppData\Local\Programs\MiKTeX\tex/latex/l3backend\l3backend-
pdftex.def) (Paper_0_Simple.aux) [1{C:/Users/le279259/AppData/Local/MiKTeX/pdft
ex/config/pdftex.map}] (Paper_0_Simple.aux) )<C:/Users/le279259/AppData/Local/P
rograms/MiKTeX/fonts/type1/public/amsfonts/cm/cmr10.pfb>
Output written on Paper_0_Simple.pdf (1 page, 13588 bytes).
Transcript written on Paper_0_Simple.log.

Finished building the pdf document with LaTeX.

#-------------------------------------------------

```
LaTeX tends to produce a verbose description, 
which may include a sometimes cryptic description of errors, 
however, if it works, you should see a pdf file
called ```Paper_0_Simple.pdf```.

## An Example with Sections

Now consider the following script, called ```Paper_1_Sections.tex```.

```
\documentclass[11pt]{article}
\begin{document}

This is my document.
The introduction is in the beginning in Section \ref{sec:intro}. 
Next is the middle, in Section \ref{sec:middle}. 
We conclude at the end in Section \ref{sec:conc}. 

\section{Introduction} \label{sec:intro}

This is the introduction. 

\section{Middle} \label{sec:middle}

This is the middle. 

\section{Conclusion} \label{sec:conc}

This is the conclusion. 


\end{document}
```

This document separates text into sections and can be build similarly
with the script ```Build_1_Sections.sh```.


## A Modular Approach to Coding Documents


A good way of organizing your work when generating documents automatically
is to enter the code for the table in a separate script and
include it in the document using the ```input``` command. 


We import some text and the table in a script called ```Paper_2_Separate.tex```.

```
\documentclass[11pt]{article}


\begin{document}


\input{Text/abstract.tex}

\pagebreak
\input{Text/introduction.tex}

\pagebreak
\input{Text/middle.tex}

\pagebreak
\input{Text/conclusion.tex}


\end{document}
```


The ```input``` command can be used to ```input``` all kinds of scripts and is useful for dividing your document into smaller parts.
In the example above, 
we use an ```input``` statement for each section of the document
and write the sections separately. 
To keep things organized, the separate text files
are stored in a separate folder called ```Text```. 
Open those files to see what's inside. 


This document can be build similarly
with the script ```Build_1_Sections.sh```.


The ```pdflatex``` 
command is shown twice because there are references to the tables,
figures and equations in the script. 
The first time it will produce a document with question marks 
in place of all the reference numbers. 
After this accounting exercise, the numbers are filled in
in the second pass. 
You will see the record of these operations in the output, 
which includes the following statement after the first pass.

```

LaTeX Warning: Reference `sec:intro' on page 1 undefined on input line 2.


LaTeX Warning: Reference `sec:middle' on page 1 undefined on input line 3.


LaTeX Warning: Reference `sec:conc' on page 1 undefined on input line 4.

) [1{C:/Users/le279259/AppData/Local/MiKTeX/pdftex/config/pdftex.map}]
(Text/introduction.tex) [2] (Text/middle.tex) [3] (Text/conclusion.tex)
[4] (Paper_2_Separate.aux)

LaTeX Warning: There were undefined references.


LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.


```

After we run the second time, LaTeX now knows the numbers of the sections to 
update the references. 
If you ran it only once, you would see instances of "Section ?"
throughout the document. 

With these skills, 
the script ```DoWork.sh``` is a similar example 
that you can use for Problem Set 2. 
