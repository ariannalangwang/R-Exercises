###############################################
## Decision Trees and Random Forests Project ##
###############################################

# For this project we will be exploring the use of tree methods to classify 
# schools as Private or Public based off their features.

# Let's start by getting the data which is included in the ISLR library, 
# the College data frame.

# A data frame with 777 observations on the following 18 variables.

# Private - A factor with levels No and Yes, indicating private or public university
# Apps - Number of applications received
# Accept - Number of applications accepted
# Enroll - Number of new students enrolled
# Top10perc - Pct. new students from top 10% of H.S. class
# Top25perc - Pct. new students from top 25% of H.S. class
# F.Undergrad - Number of fulltime undergraduates
# P.Undergrad - Number of parttime undergraduates
# Outstate - Out-of-state tuition
# Room.Board - Room and board costs
# Books - Estimated book costs
# Personal - Estimated personal spending
# PhD - Pct. of faculty with Ph.D.’s
# Terminal - Pct. of faculty with terminal degree
# S.F.Ratio - Student/faculty ratio
# perc.alumni - Pct. alumni who donate
# Expend - Instructional expenditure per student
# Grad.Rate - Graduation rate

####
## Get the Data
####
library(ISLR)

# check data sets available in package "ISLR"
data(package='ISLR')

head(College)

# reassign College to a dataframe called df
df <- College
head(df)

####
## Exploratory Data Analysis  
####
library(tidyverse)

# Create a scatterplot of Grad.Rate versus Room.Board, colored by the Private.
names(df)
ggplot(data=df, aes(x=Room.Board, y=Grad.Rate)) + geom_point(aes(color=Private))

# Create a histogram of full time undergrad students, color by Private.
ggplot(data=df, aes(x=F.Undergrad)) + geom_histogram(aes(fill=Private))

# Create a histogram of Grad.Rate colored by Private. You should see something odd here.
ggplot(data=df, aes(x=Grad.Rate)) + geom_histogram(aes(fill=Private), color='black', bins=40)
# Some colleges have Grad.Rate > 100%


# What college had a Graduation Rate of above 100% ?
head(df$Grad.Rate)
str(df$Grad.Rate)

# Note: df %>% filter(Grad.Rate > 100) does not give out the name of the college 
#       since the college names are not under a column. So use base R for this:
df[df$Grad.Rate>100, ]
# Cazenovia College

# OR:
subset(df, Grad.Rate>100)

# Change that college's grad rate to 100%.
df['Cazenovia College', 'Grad.Rate'] <- 100

 
####
## Train Test Split 
####

# Split data into training and testing sets 70/30. Use the caTools library to do this.
library(caTools)
set.seed(101) 
split = sample.split(df$Private, SplitRatio = 0.7)
train = subset(df, split == TRUE)
test = subset(df, split == FALSE)


####
## Decision Tree
####
# Use the rpart library to build a decision tree to predict whether or not a school is Private. 
# Remember to only build your tree off the training data.
library(rpart)
tree <- rpart(Private ~., method='class', data = train)

# Use predict() to predict the Private label on the test data.
predictions <- predict(tree, newdata=test)

# Check the Head of the predicted values. 
# You should notice that you actually have two columns with the probabilities.
head(predictions)

# Turn these two columns into one column to match the original Yes/No Label for a Private column.
class(predictions)
# matrix
dim(predictions)
 
joiner <- function(matrix){
  out <- NULL
  for(i in 1:nrow(matrix)){
    if(matrix[i,2]>=matrix[i,1]){
      out[i] = 'Yes'
    }else{
      out[i] = 'No'
    }
  }
  return(out)
}

predictions <- joiner(predictions)
predictions

# OR:
predictions <- as.data.frame(predictions)
 
joiner <- function(x){
  if (x>=0.5){
    return('Yes')
  }else{
    return("No")
  }
}
 
predictions <- sapply(predictions$Yes, joiner)
predictions


# Make a data frame of prediction values and real y values from the test dataset:
predictions_df <- data.frame(predictions, test$Private )

# missclassification rate:
missclassification_rate <- mean(predictions != test$Private)
missclassification_rate

# Now use table() to create a confusion matrix of your tree model:
# Note: for confusion matrix, it's table(y, y_hat)
table <- table(test$Private, predictions)
rownames(table) <- c('real no', 'real yes')
colnames(table) <- c('predicted no', 'predicted yes')
table


# Use the rpart.plot library and the prp() function to plot out your tree model.
library(rpart.plot)
prp(tree)


####
## Random Forest
####

# Call the randomForest package library
library(randomForest)

# Use randomForest() to build out a model to predict Private class. 
# Add importance=TRUE as a parameter in the model. 
# (Use help(randomForest) to find out what this does.
 
rf.model <- randomForest(Private ~ . , data = train, importance = TRUE)
# importance: Should importance of predictors be assessed?

# What was your model's confusion matrix on its own training set? 
rf.model$confusion

# Grab the feature importance with model$importance.
rf.model$importance

# Predictions
# Now use your random forest model to predict on your test set!
predictions <- predict(rf.model, newdata = test)

# confusion table
table <- table(test$Private, predictions)
table
rownames(table) <- c('real no', 'real yes')
colnames(table) <- c('pred no', 'pred yes')
table

# It should have performed better than just a single tree. 
# How much better depends on whether you are measuring 
# recall, precision, or accuracy as the most important measure of the model.