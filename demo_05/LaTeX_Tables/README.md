# Automatic Document Generation


## Tables

A table is added with the ```table``` environment. 
The notation takes some getting used to but the following script was
generated automatically using the ```xtable``` package in ```R```. 
Life is too short to type this sort of thing manually. 


We add a table to a script called ```Paper_w_Table.tex```.

```
\documentclass[11pt]{article}

\begin{document}
This is my document.

In Table \ref{tab:summary}, there are some numbers. 

\begin{table}[ht]
\centering
\begin{tabular}{rlrr}
  \hline
 & Statistic & Variable 1 & Variable 2 \\ 
  \hline
  1 & Min. & 0.19 & 0.08 \\ 
  2 & Mean & 0.70 & 0.10 \\ 
  3 & S.D. & 0.18 & 0.01 \\ 
  4 & Max. & 1.09 & 0.13 \\ 
   \hline
\end{tabular}
\caption{Summary of Numeric Variables} 
\label{tab:summary}
\end{table}


\end{document}
```



A good way of organizing your work when generating documents automatically
is to enter the code for the table in a separate script and
include it in the document using the ```input``` command. 


We import some text and the table in a script called ```Paper_w_Table_Separate.tex```.

```
\documentclass[11pt]{article}

\begin{document}

\input{../Text/my_text.tex}

\input{../Tables/my_table.tex}


\end{document}
```
The content within the ```table``` environment is in the file called ```my_table.tex```. 
The ```input``` command can be used to ```input``` all kinds of scripts and is useful for dividing your document into smaller parts.
For example, you could use an ```input``` statement for each section of the document
and write the sections separately. 

In the next demo, we will use this framework to build files from tables that we generate automatically with R.
This avoids much of the manual labor in the production of tables. 
It also makes the analysis replicable and mitigates the potential for human error.


