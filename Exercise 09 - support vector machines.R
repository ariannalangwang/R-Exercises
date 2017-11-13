#################################
#### Support Vector Machines ####
#################################

####
## Get The Data 
####
library(ISLR)
head(iris)


####
## Building the Model
####

# Train Test Split
library(caTools)
set.seed(101)
split <- sample.split(iris$Species, SplitRatio = 0.70)
train = subset(iris, split == TRUE)
test = subset(iris, split == FALSE)

# build the model:
install.packages('e1071')
library(e1071)

# Need to read this documentation!
help('svm')

model <- svm(Species ~ ., data=train)
summary(model)

# Predictions
predictions <- predict(model, newdata=test[ ,1:4])

# confusion table
table(test[ ,5], predictions)



####
## Advanced Tuning
####

# We can try to tune parameters to attempt to improve our model. We use the tune function.

# Need to read this documentation!
help('tune')
# This generic function tunes hyperparameters of statistical methods 
# using a grid search over supplied parameter ranges.

# Tune for combos of costs=0.1 , 10 , 100 and gamma=0.5, 1, 2:
# cost - allow a soft margin (allow some points to be placed on the wrong side of the margin)
# gamma - a parameter for non-linear kernel function. Large gamma leads to low variance and high bias model.

tuned_results <- tune(svm, train.x=train[ ,1:4],train.y=train[ ,5], kernel='radial',
                     ranges=list(cost=10^(-1:2), gamma=c(0.5,1,2)))
 
summary(tuned_results)
# best parameters:
# cost gamma
#    1   0.5

# We can now see that the best performance occurs with cost=1 and gamma=0.5. 
# We could try to train the model again with these specific parameters in hopes of having a better model:

tuned_model <- svm(Species ~ ., data=train, kernel="radial", cost=1, gamma=0.5)

summary(tuned_model)


# predictions
tuned_predictions <- predict(tuned_model, newdata=test[ ,1:4])
  
# new confusion table after the model being tuned with parameter cost and parameter gamma:
table(test[ ,5], tuned_predictions)

# Looks like we weren't able to improve on our model! 
# The concept of trying to tune for parameters by just trying many combinations is known as a grid search. 
# In this case, we likely have too little data to actually improve our model through careful parameter selection.

