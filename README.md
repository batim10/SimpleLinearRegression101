# SimpleLinearRegression101
My R package is an interactive, self contained function designed to walk a new R user through basic data analysis and simple linear regression.
Using mostly Y and N prompts, the user is given examples to identify data types (ordinal, continuous etc.) and has their data automatically loaded if is appropriate for Simple Linear Regression.
Once loaded, user is guided through tests to help indentify general distribution (normal, left or right skewed) of their data to identify if can be 
evaluated for the generation of correlation coefficients and construction of a model based on simple linear regression. 
User is alerted if at any point their data does not seem appropriate for the proposed testing
If a linear model is created, it is checked for validity interactively with the user and transformation is performed on user data automatically if needed 
If a valid model is created, or is appropriate for construction of a bootstrapped linear model, Confidnece Intervals are calculated 

This will be helpful to new users of R who aren't familiar with the language but would like to create a simple linear model

#You can install the development version 
devtools::install_github("batim10/SimpleLinearRegression101")
