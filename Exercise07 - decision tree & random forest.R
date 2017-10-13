#######################################
## Decision Trees and Random Forests ##
#######################################

install.packages('rpart')
library(rpart)
# rpart(formula, data=, method=,control=) where
# the formula is in the format: outcome ~ predictor1+predictor2+predictor3+ect.
# data= specifies the data frame
# method= "class" for a classification tree
# "anova" for a regression tree
# control= optional parameters for controlling tree growth.
# For example, control=rpart.control(minsplit=30, cp=0.001) requires that 
# the minimum number of observations in a node be 30 before attempting a split 
# and that a split must decrease the overall lack of fit by a factor of 0.001 
# (cost complexity factor) before being attempted.






