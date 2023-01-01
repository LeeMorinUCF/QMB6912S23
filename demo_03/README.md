# The Box-Cox Transformation

The Box-Cox power transformation refers to a way of
transforming a response (dependent) variable to satisfy the usual
regression assumption of homogeneity and normality of variance. 
A regression model is then used to fit the transformed response variable. 

This framework was analyzed in detail by the legendary statisticians
[George Box](https://en.wikipedia.org/wiki/George_E._P._Box)
and 
[Sir David Cox](https://en.wikipedia.org/wiki/David_Cox_(statistician))
in their article 
"An Analysis of Transformations" published in
the *Journal of the Royal Statistical Society*
in 1964.



[Statistics How To](https://www.statisticshowto.com/) 
shows some [definitions and examples of the Box Cox transformation](https://www.statisticshowto.com/box-cox-transformation/#:~:text=What%20is%20a%20Box%20Cox%20Transformation%3F%20A%20Box,able%20to%20run%20a%20broader%20number%20of%20tests.)
In particular, they show a table highlighting 
the commonly-used transformations as special cases of the Box Cox transformation.

The Website [Towards Data Science](https://towardsdatascience.com/)
includes an article explaining the transformation and implementing it in Python: 
[Box-Cox Transformation: Explained](https://towardsdatascience.com/box-cox-transformation-explained-51d745e34203)

The Website [SPC for Excel](www.spcforexcel.com)
gives a demonstration of the practical steps involved in
determining the degree of improvement after performing the Box-Cox transformation
on the Webpage [Box-Cox Transformation](https://www.spcforexcel.com/knowledge/basic-statistics/box-cox-transformation).


The Website [The Business Professor](https://thebusinessprofessor.com/)
offers a brief description of [how the transformation is used](https://thebusinessprofessor.com/en_US/research-analysis-decision-science/box-cox-transformation-definition)
within a regression model. 
One of the important issues is that now the model predicts
the transformation of the variable, 
rather than the variable itself, 
so it complicates the interpretation of the regression model. 
These points are also addressed in a review article
in The Statistician
entitled
[The Box-Cox transformation technique: a review](http://staff.ustc.edu.cn/~zwp/teach/Reg/Boxcox.pdf).

R users might appreciate the example in 
the Website [Statology](www.statology.org)
in an article called 
[How to Perform a Box-Cox Transformation in R (With Examples)](https://www.statology.org/box-cox-transformation-in-r/). 

The Website [Encyclopedia of Mathematics](https://encyclopediaofmath.org/wiki/Main_Page)
offers a more technical description
of the [Box-Cox transformation](https://encyclopediaofmath.org/wiki/Box-Cox_transformation), 
including the relationship to a family of distributions 
called the exponential family, 
many of which you might encounter in an advanced course in statistics. 

Some of the key implications of the theory are that
- the transformation parameter must be estimated by maximum likelihood
- this estimation is often performed by numerical optimization of a likelihood function
- one must take care to correctly formulate the likelihood function
- one important consideration is the Jacobian term
that results from the transformation of a variable:
the density of the original variable is scaled by a factor relating to the derivative of the transformation function. 

