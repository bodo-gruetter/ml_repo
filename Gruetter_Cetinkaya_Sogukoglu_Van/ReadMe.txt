Group 13:
Bodo Gruetter, bodojeremy.gruetter@stud.hslu.ch
Necati Van, necati.vanr@stud.hslu.ch
Cengiz Cetinkaya, cengiz.cetinkaya@stud.hslu.ch
Malik Sogukoglu, malik.sogukoglu@stud.hslu.ch

Our work consists of four files.

1. group13_bikesharing.R
The R-file contains all R-code of all analyses performed.
It partly includes analyses that were not included in the documentation (.html).
In order to execute the code it must be ensured that the following packages are installed and imported:

library(ggplot2)
library(e1071)
library(mgcv)
library(corrplot)
library(tree)
library(mgcv)
library(tidyverse)
library(boot)
library(MASS)
library(ipred)
library(randomForest)
library(gbm)
library(modelr)
library(neuralnet)
library(h2o)
library(bit64)
library(caret)

2. group13_bikesharing.Rmd
The R-Markdown File contains our documentation with preprocessing, explorative data analysis,
modeling and comparison of the models. In order for it to be executed,
it must be ensured that all packages (see point 1) are installed and imported.

3. group13_bikesharing.html
The HTML document includes the final output with preprocessing, explorative data analysis,
modeling and comparison of the models.

4. bikesharing.csv
The CSV file contains the complete data set that was used for analysis.