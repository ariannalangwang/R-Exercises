#########################################
#### Support Vector Machines Project ####
#########################################

# For this project we will be exploring publicly available data from LendingClub.com. 
# Lending Club connects people who need money (borrowers) with people who have money (investors). 
# Hopefully, as an investor you would want to invest in people who showed a profile of having a high probability 
# of paying you back. We will try to create a model that will help predict this.

# We will use lending data from 2007-2010 and try to classify and predict whether or not the borrower paid back the loan in full. 

# Here are what the columns represent:
# credit.policy: 1 if the customer meets the credit underwriting criteria of LendingClub.com, and 0 otherwise.
# purpose: The purpose of the loan (takes values "credit_card", "debt_consolidation", "educational", "major_purchase", "small_business", and "all_other").
# int.rate: The interest rate of the loan, as a proportion (a rate of 11% would be stored as 0.11). Borrowers judged by LendingClub.com to be more risky are assigned higher interest rates.
# installment: The monthly installments owed by the borrower if the loan is funded.
# log.annual.inc: The natural log of the self-reported annual income of the borrower.
# dti: The debt-to-income ratio of the borrower (amount of debt divided by annual income).
# fico: The FICO credit score of the borrower.
# days.with.cr.line: The number of days the borrower has had a credit line.
# revol.bal: The borrower's revolving balance (amount unpaid at the end of the credit card billing cycle).
# revol.util: The borrower's revolving line utilization rate (the amount of the credit line used relative to total credit available).
# inq.last.6mths: The borrower's number of inquiries by creditors in the last 6 months.
# delinq.2yrs: The number of times the borrower had been 30+ days past due on a payment in the past 2 years.
# pub.rec: The borrower's number of derogatory public records (bankruptcy filings, tax liens, or judgments).


####
## Get The Data 
####
library(tidyverse)

# Open the loan_data.csv file and save it as a dataframe called loans:
loans <- read.csv('loan_data.csv')

# Check the summary and structure of loans.
summary(loans)
str(loans)

# Convert the following columns to categorical data using factor()
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)
loans$credit.policy <- factor(loans$credit.policy)

str(loans)



####
## Exploratory Data Analysis (EDA)
####

# Create a histogram of fico scores colored by not.fully.paid
ggplot(data=loans, aes(x=fico)) + geom_histogram(aes(fill=not.fully.paid), color='black', bins=50)

# Create a barplot of purpose counts, colored by not.fully.paid. Use position='dodge' in the geom_bar argument
ggplot(data=loans, aes(x=purpose)) + geom_bar(aes(fill=not.fully.paid), position='dodge') + 
       theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create a scatterplot of fico score versus int.rate. Does the trend make sense? 
# Play around with the color scheme to make the plot more beautiful.
ggplot(data=loans, aes(x=int.rate, y=fico)) + geom_point(aes(color=not.fully.paid))
# The trend makes sense: the higher the fico scores, the lower the interest rates.



####
## Building the Model
####

# Train Test Split
library(caTools)
set.seed(101)
split <- sample.split(loans$not.fully.paid, SplitRatio = 0.70)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

# build the model:
# install.packages('e1071')
library(e1071)

# Use the svm() function to train a model on your training set.
help('svm')
model <- svm(not.fully.paid ~ . , data=train)

# Get a summary of the model.
summary(model)

# Use predict to predict new values from the test set using your model. 
predictions <- predict(model, newdata=subset(test, select=c(-not.fully.paid)))
# predict(model, newdata = test data_features)

# make a confusion table
table(test$not.fully.paid, predictions)

#     predictions
#        0    1
#  0  2413    0
#  1   460    0

# Bad results! With the model classifying everything into one group! 
# Let's tune our model to try to fix this.



####
## Tuning the Model
####

# Use the tune() function to test out different cost and gamma values.
# Note: In the lecture we showed how to do this by using train.x and train.y, 
#       but its usually simpler to just pass the formula of the model.
tuned_svm <- tune(svm, train.x = not.fully.paid ~ . , data = train,  kernel='radial',
                     ranges=list(cost=c(1,10), gamma=c(0.1,1)))

summary(tuned_svm)


# We can now see that the best performance occurs with cost=10 and gamma=0.1 
# We could try to train the model again with these specific parameters in hopes of having a better model:
tuned_model <- svm(not.fully.paid ~ ., data=train, cost=10, gamma = 0.1)
summary(tuned_model)

# predictions
tuned_predictions <- predict(tuned_model, newdata=subset(test, select=c(-not.fully.paid)))

# new confusion table after the model being tuned with parameter cost and parameter gamma:
table(test$not.fully.paid, tuned_predictions)


































