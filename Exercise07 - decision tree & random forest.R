#######################################
## Decision Trees and Random Forests ##
#######################################

####
## Growing a single Decision Tree
####

install.packages('rpart')
library(rpart)

# rpart(formula, data=, method=, control=) where
# the formula is in the format: response ~ predictor1 + predictor2 + ect.
# data = data frame
# method= "class" for a classification tree
#         "anova" for a regression tree
# control= optional parameters for controlling tree growth.
# For example, control=rpart.control(minsplit=30, cp=0.001) requires that 
# the minimum number of observations in a node be 30 before attempting a split 
# and that a split must decrease the overall lack of fit by a factor of 0.001 
# (cost complexity factor) before being attempted.

####
## Get the Data
####

# We'll use the kyphosis data frame which has 81 rows and 4 columns, 
# representing data on children who have had corrective spinal surgery. 
# It has the following columns:
# Kyphosis - a factor with levels absent and present, indicating if a kyphosis was present after the operation.
# Age - in months
# Number - the number of vertebrae involved
# Start - the number of the first (topmost) vertebra operated on.

head(kyphosis)
str(kyphosis)


####
## Build a single classification decision tree
####
tree <- rpart(Kyphosis ~ . , method='class', data= kyphosis)


####
## Tree Visualization
####
install.packages('rpart.plot')
library(rpart.plot)
 
prp(tree)

######################################################################################

####
## Random Forests
####

# Random forests improve predictive accuracy by generating a large number of bootstrapped trees -
# random samples with replacements and also random samples of m variables out of a total of p variables.
# We then classify a case using each tree, and decide on a final predicted outcome by combining the results 
# across all of the trees (an average in regression, a majority vote in classification).
# 
# We can use the randomForest library to create and build out a Random Forest:

install.packages('randomForest')
library(randomForest)

model <- randomForest(Kyphosis ~ .,   data=kyphosis)

# view results
print(model) 

# see what kind of names we can call with $
names(model) 

# for example:
model$err.rate  # error rates
model$ntree  # number of trees
model$confusion  # confusion matrix
model$importance  # importance of each predictor

# The key is to just understand the background of the algorithm being used and 
# know what library to install and use for the specific algorithm being used.











