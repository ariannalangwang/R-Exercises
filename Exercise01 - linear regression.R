###########################
#### LINEAR REGRESSION ####
###########################
####
## Get our Data 
####
library(readr)
# Read the file in using R-Studio
df <- read.csv('student-mat.csv', sep=';')  # Before running this line, set working directory in the RStudio Files panel -> More
                   
head(df)
tail(df)
str(df)
summary(df)

####
## Clean the Data  
####
any(is.na(df)) # is.na(object) returns a vector or a matrix.
# False
# class(is.na(df))  # matrix

####
## Exploratory Data Analysis  
####

# correlation
num.cols <- sapply(df, is.numeric)  # Grab only numeric columns
num.cols  # num.cols is a vector of TRUE and FALSE
df[num.cols] # new dataframe with only numeric columns
cor.data <- cor(df[num.cols])

# corrplot
install.packages('corrplot')
library(corrplot)
corrplot(cor.data, method='color')

# corrgram (corrgram allows passing in the dataframe directly)
install.packages('corrgram') 
library(corrgram)
corrgram(df)

# We're trying to predict the G3 score. So let's see a histogram of these scores:
library(tidyverse)
ggplot(df, aes(x=G3)) + geom_histogram(bins=20, alpha=0.5, fill='blue') + theme_minimal()

####
## Train and Test Data
####
library(caTools)
set.seed(101) 
sample <- sample.split(df$G3, SplitRatio = 0.70)  # df$G3: can choose any column. Choose the y column by convention.
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)

# Train our Model
model <- lm(G3 ~ ., data=train)
summary(model)

names(model)  # see other functions of the model
# for example, compute fitted values of the model
fitted.values(model)
residuals(model)

# use ggplot to plot the model
plt1 <- ggplot(data=train, aes(x = train$absences, y = train$G3)) + geom_point() 
plt2 <- plt1 + stat_smooth(method = "lm", col = "red")
print(plt2)

# Grab residuals
res <- residuals(model)

# Convert to DataFrame for gglpot
res <- as.data.frame(res)
head(res)

# We want a histogram of our residuals to be normally distributed, 
# something with a strong bimodal distribution may be a warning that 
# our data was not a good fit for lienar regression.
ggplot(data=res, aes(x=res)) +  geom_histogram(fill='blue',alpha=0.5)

# Looks like there are some suspicious residual values that have a value less than -5. 
# We can further explore this by just calling plot on our model.
# Regression Validation - four-plots to see the fit of the model:
plot(model)

# Predictions
# test our model by predicting on our testing set.
G3_predictions <- predict(model, data=test)  # gives all the y_hat valeus in the test set.

# Now we can get the root mean squared error (square root of MSE), 
# a standardized measure of how off we were with our predicted values.
results <- cbind(G3_predictions, test$G3) 
colnames(results) <- c('pred','real')
results <- as.data.frame(results)

# Now let's take care of negative predictions!
to_zero <- function(x){
  if  (x < 0){
    return(0)
  }else{
    return(x)
  }
}
 
results$pred <- sapply(results$pred,to_zero)

# Compute MSE (mean squared error):
MSE <- mean((results$real-results$pred)^2)
print(MSE)

# Compute R-Squared Value for our model (just for the predictions)
SSE <- sum((results$real-results$pred)^2)
SST <- sum((results$real-mean(test$G3))^2) 
R2 <- 1 - SSE/SST
print(R2)





 