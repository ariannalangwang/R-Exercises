#####################################################
#### Unsupervised Learning - Neural Nets Project ####
#####################################################

# We'll use the Bank Authentication Data Set from the UCI repository.
# The data consists of 5 columns:
# variance of Wavelet Transformed image (continuous)
# skewness of Wavelet Transformed image (continuous)
# curtosis of Wavelet Transformed image (continuous)
# entropy of image (continuous)
# class (integer)
# Where class indicates whether or not a Bank Note was authentic. Class is our response variable.


####
## Get The Data 
####

# Use read.csv to read the bank_note_data.csv file.
df <- read.csv('bank_note_data.csv')

# Check the head of the data frame and its structure & summary.
head(df)
str(df)
summary(df)


####
## Train Test Split 
####

library(caTools)
set.seed(101)
split = sample.split(df$Class, SplitRatio = 0.70)  # need scaled_df to be a data frame.
train = subset(df, split == TRUE)
test = subset(df, split == FALSE)


# Check the structure of the train data and note that Class is still an int data type. 
# We won't convert it to a factor for now because the neural net requires all numeric information.
str(train) 
# 'data.frame':	960 obs. of  5 variables:
#   $ Image.Var : num  1.1214 1.4465 1.0634 -0.0368 1.3841 ...
# $ Image.Skew: num  1.15 1.06 1.3 -1.09 1.32 ...
# $ Image.Curt: num  -0.976 -0.895 -1.255 0.736 -1.243 ...
# $ Entropy   : num  0.3544 -0.1287 -1.1436 0.0966 -0.938 ...
# $ Class     : num  -0.894 -0.894 -0.894 -0.894 -0.894 ...



####
## Building the Neural Net
####

library(neuralnet)
#help(neuralnet)

# Use the neuralnet function to train a neural net, 
# set linear.output=FALSE and choose 10 hidden neurons (hidden=10)
names(df)
nn <- neuralnet(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy, data=train, hidden=10, linear.output=FALSE)


####
## Predictions
####

predicted.nn.values <- compute(nn, test[ ,1:4])

# the predicted values are:
predicted.nn.values$net.result


# You should notice that the predicted values are still probabilities.
# Apply the round function to the predicted values so you only 0s and 1s as your predicted classes.
predictions <- sapply(predicted.nn.values$net.result, round)

head(predictions)


# Use table() to create a confusion matrix of your predictions versus the real values
table(test$Class, predictions)
# Noticed that we did very well! Almost suspiciously well! 
# Let's check our results against a randomForest model!



####
## Comparing Models
####

# Compare to a random forest model
# Call the randomForest library
library(randomForest)

# Set the Class column of the data as a factor (randomForest needs it to be a factor, not an int like neural nets did.)
# Then re-do the train/test split

df$Class <- factor(df$Class)

library(caTools)
set.seed(101)
split = sample.split(df$Class, SplitRatio = 0.70)
train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

# Create a randomForest model with the new adjusted training data.
model <- randomForest(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy, data=train)

# Use predict() to get the predicted values from your rf model.
rf.pred <- predict(model, test)

# Use table() to create the confusion matrix.
table(test$Class, rf.pred)

# How did the models compare?
# The neural nets model seems to perform a bit better than the random forest model.


















