############################################
## K Nearest Neighbors Classifier Project ##
############################################

# This project is a practice of using K Nearest Neighbors Classifier algorithm to 
# attempt to classify the species of iris flowers based on four features of the flower.

library(ISLR)
library(tidyverse)

####
## Get the Data
####
head(iris)
str(iris)


####
## Standardize the Data
####

# It's important to standardize the features in the data frame in order to use the KNN method:
features <- iris %>% select(-Species)
response <- iris %>% select(Species)
standard_features <- scale(features)

# Check the standarization worked:
var(standard_features[ ,1])
 
# Join the standardized data with the response column to make a standardized final data frame:
final_data <- cbind(standard_features, response)
head(final_data, 3)


####
## Train and Test Splits
####

library(caTools)
set.seed(101)

split <- sample.split(final_data$Species, SplitRatio = .70)
train <- subset(final_data, split == TRUE)
test <- subset(final_data, split == FALSE)


####
## Build a KNN model
####

# knn() function returns a vector of predicted Y’s.
# The first argument is the train.features (a data frame)
# the second argument is the test.features (a data frame)
# the third argument is the train.Y (must be a vector)
# the fourth argument is the k (how many neighbors).

library(class)
set.seed(101)
# Use the knn function to predict Species of the test set. Use k=1.
predicted_species <- knn(train[ ,1:4], test[ ,1:4], train$Species, k=1)

# What was your misclassification rate?
missclassification_rate <- mean(predicted_species != test$Species)
# 0.04444444


####
## Choosing a K Value  
####

# Create a plot of the error (misclassification) rate for k values ranging from 1 to 20:
predicted_species <- NULL
missclassification_rate <- NULL
for(i in 1:20){
  set.seed(101)
  predicted_species <- knn(train[ ,1:4], test[ ,1:4], train$Species, k=i)
  missclassification_rate[i] <- mean(predicted_species != test$Species)
}
missclassification_rate

# Make a data frame of k_values and missclassification_rate:
k_values <- 1:20
k_value_df <- data.frame(k_values, missclassification_rate)

# Elbow Method
# We can plot out the various error rates for the K values. 
# We should see an "elbow" indicating that we don't get a decrease 
# in error rate for using a higher K. This is the good cut-off point.

ggplot(data=k_value_df, aes(x=k_values, y=missclassification_rate) ) + geom_point() + geom_line(color='red')

# Missclassification rate drops to its lowest for k values between 2-6. 
# Then it begins to jump back up again, this is due to how small the data set it. 
# At k=10 you begin to approach setting k=10% of the data, which is quite large. 

# So choose k values between 2-6 is the best because it gives the smallest missclassification rate.














s























