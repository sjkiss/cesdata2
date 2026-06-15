#### Recode script for ces25 most important problem
# load dataset
data("ces25")
# load ces21
# for comparison
data("ces21")
library(labelled)
library(haven)
table(as_factor(ces21$mip))
