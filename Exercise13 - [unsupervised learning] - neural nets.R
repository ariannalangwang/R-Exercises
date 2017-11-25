#############################################
#### Unsupervised Learning - Neural Nets ####
#############################################
 
# We will use the popular Boston dataset from the MASS package, 
# which describes some features for houses in Boston in 1978.

# CRIM - per capita crime rate by town
# ZN - proportion of residential land zoned for lots over 25,000 sq.ft.
# INDUS - proportion of non-retail business acres per town
# CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)
# NOX - nitric oxides concentration (parts per 10 million)
# RM - average number of rooms per dwelling
# AGE - proportion of owner-occupied units built prior to 1940
# DIS - weighted distances to five Boston employment centres
# RAD - index of accessibility to radial highways
# TAX - full-value property-tax rate per 10,000 dollars
# PTRATIO - pupil-teacher ratio by town
# B - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
# LSTAT - % lower status of the population
# MEDV - Median value of owner-occupied homes in $1000's

# We will be trying to predict the Median Value MEDV



####
## Get The Data 
####

library(MASS)

set.seed(101)
data <- Boston

str(data)
summary(data)
head(data)

any(is.na(data))
# FALSE




####
## Neural Net Model 
####

install.packages('neuralnet')
library(neuralnet)


####
## Training the Model 
####

# It is good practice to normalize the data before training a neural network. 
# Depending on the dataset, avoiding normalization may lead to useless results or 
# to a very difficult training process. (most of the times the algorithm will not 
# converge before the number of maximum iterations allowed). 

# You can choose different methods to scale the data (z-normalization, min-max scale, etc…). 
# Usually scaling in the intervals [0,1] or [-1,1] tends to give better results. 


# In this particular case, I will scale the data using: scaled_data = (data-min) / (max-min)
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

maxs
# crim       zn    indus     chas      nox       rm      age      dis      rad      tax  ptratio    black    lstat     medv 
# 88.9762 100.0000  27.7400   1.0000   0.8710   8.7800 100.0000  12.1265  24.0000 711.0000  22.0000 396.9000  37.9700  50.0000 

mins
# crim        zn     indus      chas       nox        rm       age       dis       rad       tax   ptratio     black     lstat      medv 
# 0.00632   0.00000   0.46000   0.00000   0.38500   3.56100   2.90000   1.12960   1.00000 187.00000  12.60000   0.32000   1.73000   5.00000 


scaled_data <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
head(scaled_data)


# Train and Test Sets 
library(caTools)
set.seed(101)
split = sample.split(scaled_data$medv, SplitRatio = 0.70)
train = subset(scaled_data, split == TRUE)
test = subset(scaled_data, split == FALSE)

# Formula for Neural Net
# For some odd reasons, the neuralnet() function won't accept a formula in the form: y~. that we are used to using. 
# Instead you have to call all the columns added together.

# Get column names
n <- names(train)

# Paste together
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
f
# medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat


# the neuralnet model:
nn <- neuralnet(f, data=train, hidden=c(5,3), linear.output=TRUE)

# hidden=c(5,3) <- first layer has 5 nodes, 3 layers in total.
# linear.output=TRUE <- the output is a continuous variable. If the output is a categorical variable (a classification problem), then linear.output=FALSE.




####
## Neural Net Visulization
####

# You can plot out your model to see a very neat visualization with the weights on each connection.

# The black lines show the connections between each layer and the weights on each connection 
# while the blue lines show the bias term added in each step. The bias can be thought as the intercept of a linear model. 

# The net is essentially a black box so we cannot say that much about the fitting, the weights and the model. 
# Suffice to say that the training algorithm has converged and therefore the model is ready to be used.

plot(nn)




####
## Predictions
####

# Now we can predict the values for the test set and compute the MSE. 
# Remember that the net will output a normalized prediction, so we need to scale it back.

# Compute Predictions off Test Set
predicted_values <- compute(nn, test[1:13])
# Note neuralnet uses 'compute' not 'predict' to make predictions.

# It returns a list
str(predicted_values)

# the predicted values are:
predicted_values$net.result


# Convert back to non-scaled predicted values
true_predicted_values <- predicted_values$net.result * (max(data$medv)-min(data$medv)) + min(data$medv)

# Convert the test data
true_test_values <- (test$medv) * (max(data$medv)-min(data$medv)) + min(data$medv)


# Check the Mean Squared Error
MSE <- sum((true_test_values - true_predicted_values)^2) / nrow(test)
MSE 
# 15.16559995



# Visualize the Errors
error_df <- data.frame(true_test_values, true_predicted_values)
head(error_df)

library(ggplot2)
ggplot(error_df, aes(x=true_test_values, y=true_predicted_values)) + geom_point() + stat_smooth()

# Looks like a few houses threw off our model, but overall its not looking too bad considering we're pretty much treating it 
# like a total black box.




####
## Conclusion
####

# Neural networks resemble black boxes a lot: explaining their outcome is much more difficult 
# than explaining the outcome of simpler model such as a linear model. 
# Furthermore, as you have seen above, extra care is needed to fit a neural network and small changes 
# can lead to different results.