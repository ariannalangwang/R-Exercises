####################################
## K Nearest Neighbors Classifier ##
####################################

####
## Get Data
####
# We'll use the ISLR package to get the data
install.packages('ISLR')
library(ISLR)

# We will apply the KNN approach to the Caravan data set. 
# This data set has 85 predictors that measure characteristics for 5,822 individuals. 
head(Caravan)
str(Caravan)

# The response variable is Purchase, which indicates whether or not a given 
# individual purchases a Caravan insurance policy. 
# In this data set, only 6% of people purchased caravan insurance.
summary(Caravan$Purchase)

####
## Cleaning Data
####

any(is.na(Caravan))  
# False


####
## Standardize Variables
####
# The KNN classifier predicts the class of a test observation 
# by identifying the observations that are nearest to it, 
# so, the scale of the variables matters. 
# Any variables that are on a large scale will have a much larger effect 
# on the distance between the observations, and hence on the KNN classifier, 
# than variables that are on a small scale.

standardized.Caravan <- scale(Caravan[ ,-86])  # -86, the purchase column.
purchase <- Caravan[ ,86]  # Note: this is a vector

# Let's check the variance:
var(standardized.Caravan[ ,1])
# 1

####
## Training & Testing Data
####
# First 1000 rows as test data:
test.index <- 1:1000
test.data <- standardized.Caravan[test.index, ]
test.purchase <- purchase[test.index]
train.data <- standardized.Caravan[-test.index, ]
train.purchase <- purchase[-test.index]

####
## Using KNN
####

# knn() function returns a vector of predicted Y’s.
# The first argument is the train.features (a data frame)
# the second argument is the test.features (a data frame)
# the third argument is the train.Y (must be a vector)
# the fourth argument is the k (how many neighbors).

library(class)
predicted.purchase <- knn(train.data, test.data, train.purchase, k=1)
head(predicted.purchase)

# misclassification rate:
error.rate <- mean(predicted.purchase != test.purchase)

####
## Choosing K Value:
####

# Note:
# NULL represents a null object in R.
# NA is a logical constant of length 1, which contains a missing value indicator.

# Write a for loop to check misclassification rate with k-value 1-20:
predicted.purchase <- NULL  # empty object
error.rate <- NULL
for(i in 1:20){
  predicted.purchase <- knn(train.data,test.data,train.purchase,k=i)
  error.rate[i] <- mean(predicted.purchase != test.purchase)
}

error.rate

# Elbow Method
# We can plot out the various error rates for the K values. 
# We should see an "elbow" indicating that we don't get a decrease 
# in error rate for using a higher K. This is the good cut-off point.

k.values <- 1:20
error.df <- data.frame(error.rate, k.values)
ggplot(error.df, aes(x=k.values, y=error.rate)) + geom_point() + geom_line(lty="dotted", color='red')
# From the graph, we can see that beyond K=9 does not help the misclassification rate 
# at all. So we can set K=9 for our model during training.

















 