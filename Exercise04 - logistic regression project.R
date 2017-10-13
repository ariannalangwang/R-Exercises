##################################
##  Logistic Regression Project ##
##################################

# We will predict if people in the data set either making <=50k or >50k per year.
# Typically most of your time is spent cleaning data, not running the few lines 
# of code that build your model, this project will try to reflect that by showing 
# different issues that may arise when cleaning data.

#### Get the Data 
##
getwd()
setwd('/Users/langwang/Documents/****SELF-STUDY****/Udemy-Data Science and Machine Learning Bootcamp with R - Jose Portilla/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects')
getwd()
library(tidyverse)
adult <- read.csv('adult_sal.csv')
head(adult)
# Notice first colum has been repeated. Drop first column.
adult <- adult %>% select(-X)
head(adult)
str(adult)
summary(adult)
##
#### Data Cleaning
##
names(adult)
## type_employer column
# Use table() to check out the frequency of the type_employer column:
table(adult$type_employer)
# Combine two smallest groups into a single group called "Unemployed":
unemployed <- function(type_employer){
  type_employer <- as.character(type_employer)
  if (type_employer=='Never-worked' | type_employer=='Without-pay'){
    return('Unemployed')
  }else{
    return(type_employer)
  }
}
adult$type_employer <- sapply(adult$type_employer,unemployed)
# check again:
table(adult$type_employer)
 
# Combine State and Local gov jobs into a category called SL-gov and 
# combine self-employed jobs into a category called self-emp.
gov_self <- function(type_employer){
  type_employer <- as.character(type_employer)
  if(type_employer=='Local-gov' | type_employer=='State-gov'){
    return('SL-gov')
  }else if(type_employer=='Self-emp-inc' | type_employer=='Self-emp-not-inc'){
    return('self-emp')
  }
  else{
    return(type_employer)
  }
}
adult$type_employer <- sapply(adult$type_employer,gov_self)

table(adult$type_employer)
 
## Marital Column
# Use table() to look at the marital column:
table(adult$marital)
# Reduce to three groups: Married, Not-Married, Never-Married:
group_marital <- function(marital){
  marital <- as.character(marital)
  if(marital=='Divorced' | marital=='Separated' | marital=='Widowed'){
    return('Not-Married')
  } else if(marital=='Never-married'){
    return('Never-Married')
  }else{
    return('Married')
  }
}
adult$marital <- sapply(adult$marital,group_marital)
table(adult$marital)

## Country Column
table(adult$country)
levels(adult$country)
# group countries by continents:
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(country){
  if (country %in% Asia){
    return('Asia')
  }else if (country %in% North.America){
    return('North.America')
  }else if (country %in% Europe){
    return('Europe')
  }else if (country %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}
adult$country <- sapply(adult$country,group_country)
table(adult$country)
# Rename the country column to region column:
adult <- adult %>% rename(region = country)
# check structure of the data frame again:
str(adult)
# change back to categorical variables:
adult$type_employer <- sapply(adult$type_employer,factor)
adult$marital <- sapply(adult$marital,factor)
adult$country <- sapply(adult$country, factor)
# can also do:  adult$type_employer <- factor(adult$type_employer)
##
#### Missing Data
##
library(Amelia)
# Convert any cell with a '?' value to a NA value:
adult[adult == '?'] <- NA
missmap(adult, y.at=c(1), y.labels = c(''),  col=c('yellow','black'))

# Use na.omit() to omit NA data from the adult data frame:
adult <- na.omit(adult)
# Use missmap() to check that all the NA values were in fact dropped:
missmap(adult, y.at=c(1), y.labels = c(''),  col=c('yellow','black'))

##
#### EDA
##
ggplot(adult, aes(age)) + geom_histogram(aes(fill=income), color='black', binwidth=1) + theme_bw()
ggplot(adult,aes(hr_per_week)) + geom_histogram(aes(fill=income)) + theme_bw()  
ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##
#### Building the logistic Regression Model
## Train Test Split
library(caTools)
set.seed(101)
sample <- sample.split(adult$income, SplitRatio = 0.70)
train = subset(adult, sample == TRUE)
test = subset(adult, sample == FALSE)

model = glm(income ~ ., family = binomial(link=logit), data = train)
summary(model)
names(model)

## The step() function iteratively tries to remove predictor variables from the model by using AIC 
## in an attempt to delete variables that do not significantly add to the fit.
# step(object, direction = c("both", "backward", "forward") )
new.step.model <- step(model)
##
#### Prediction
##
test$predicted.income <- predict(new.step.model, newdata=test, type="response")
## Confusion Table
table <- table(test$income, test$predicted.income > 0.5)
colnames(table) <- c('predicted <=50k', 'predicted >50k')
rownames(table) <- c('true <=50k', 'true >50k')
table






























 



































