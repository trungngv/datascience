# exploratory analysis
library(DescTools)
library(gmodels)
library(PerformanceAnalytics)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(HH) # including likert chart, or diverging stacked bar chart

# some good references
#Carr, Daniel B. and Linda Williams Pickle. 2010. Visualizing Data Patterns with Micromaps. CRC Press.
#Chang, Winston. 2013. R Graphics Cookbook. O’Reilly Media.
#Cleveland, William S. 1994. The Elements of Graphing Data. 2nd ed. Hobart Press.
#Heiberger, Richard, and Burt Holland. 2015. Statistical Analysis and Data Display: An Intermediate Course with Examples in R. 2nd ed. Springer.
#Heiberger, Richard M., and Naomi B. Robbins. 2014. “Design of Diverging Stacked Bar Charts for Likert Scales and Other Applications.” Journal of Statistical Software, 57 (5).
#Jacoby, William G. 1998. Statistical Graphics for Visualizing Multivariate Data. Sage.
#Murrell, Paul. 2011. R Graphics, Second Edition. CRC Press.
#Robbins, Naomi B. 2013 [2004]. Creating More Effective Graphs. Chart House.
#Tufte, Edward R. 2001. The Visual Display of Quantitative Information. 2nd ed. Graphics Press. Wickham, Hadley. 2016. ggplot2: Elegant Graphics for Data Analysis. 2nd ed. Springer. Wilkinson, Leland. 2005. The Grammar of Graphics. 2nd ed. Springer.
#Xie, Yihui. 2015. Dynamic Documents with R and knitr, Second Edition. CRC Press.
#Hadley Wickham (2008) ggplot - Elegant Graphics for Data Analysis.pdf	added TOCS	2 years ago
#Leland Wilkinson (2005) Grammar of Graphics.pdf	added TOCS	2 years ago
#John Chambers (2008) Software for Data Analysis.pdf	added TOCS	2 years ago
#Paul Murrell (2006) R Graphics.pdf
#Deepayan Sarkar (2008) Lattice - Multivariate Dat Visualization in R-2.pdf	added TOCS	2 years ago

#-- Some settings
# turn off scientific notation globally
options(scipen=1000)
# use 3 digits for reporting
options(digits=3)

# describe data
Desc(iris)

# plot correlations for numeric variables
corr <- iris[,1:3]
chart.Correlation(corr,histogram = TRUE,pch=19)

# from gmodels
# for categorical variables
CrossTable(myxvector, myyvector, prop.t=FALSE, prop.chisq = FALSE)

# visualization with featurePlot
# http://topepo.github.io/caret/visualizations.html

