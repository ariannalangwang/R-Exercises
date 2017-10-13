#############################
#### LOGISTIC REGRESSION ####
#############################

#### Get The Data 
getwd()
library(readr)
df.train <- read.csv('titanic_train.csv')
head(df.train)
str(df.train)
summary(df.train)
## some variables should be categorical
df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$Parch <- factor(df.train$Parch)
df.train$SibSp <- factor(df.train$SibSp)
str(df.train)


##### Exploratory Data Analysis (EDA)
## Let's explore how much missing data we have, 
## we can use the Amelia pacakge for this. 
library(Amelia)
missmap(df.train, main="Titanic Training Data Missings Map", col=c("yellow", "black"), legend=FALSE) 
# col=c(NA color, non-NA color),  choose legend=FALSE because the legned it gives is big
# Also, enlarge plot space. Otherwise, may get an error message.

# Roughly 20% of the Age data is missing. 
# The proportion of Age "missings" is likely small enough for reasonable replacement with imputation.

## Data Visualization with ggplot2
library(ggplot2)
ggplot(df.train,aes(Survived)) + geom_bar()  
ggplot(df.train,aes(Pclass)) + geom_bar(aes(fill=Pclass),alpha=0.5)
ggplot(df.train,aes(Sex)) + geom_bar(aes(fill=Sex),alpha=0.5)
ggplot(df.train,aes(Age)) + geom_histogram(aes(fill=Pclass), bins=20, alpha=0.5, position = "dodge")
ggplot(df.train,aes(SibSp)) + geom_bar(fill='red',alpha=0.5)
ggplot(df.train,aes(Fare)) + geom_histogram(fill='green',color='black',alpha=0.5)
# a very telling graph of survival rate for each class:
ggplot(df.train,aes(Survived)) + geom_bar(aes(fill=Pclass), position = "dodge", alpha=0.5)  


#### Data Cleaning
## impute missing age by each passenger class's age median:
library(tidyverse)
# Note: dplyer manipulates a dataframe and returns a dataframe also.
df.train.class1 <- df.train %>% filter(Pclass==1)  
# Note: to compute the median of a column:
median(df.train.class1$Age, na.rm=T)  # 37
# OR:
# df.train.class1 <- df.train %>% filter(Pclass==1) %>% select(Age) 
# apply(df.train.class1, 2, median)
 
df.train.class2 <- df.train %>% filter(Pclass==2)  
median(df.train.class2$Age, na.rm=T)  # 29
df.train.class3 <- df.train %>% filter(Pclass==3)  
median(df.train.class3$Age, na.rm=T)  # 24

# to visualize this with a boxplot:
pl <- ggplot(df.train,aes(x=Pclass,y=Age)) + geom_boxplot(aes(group=Pclass,fill=Pclass,alpha=0.4)) 
pl + scale_y_continuous(breaks = seq(0, 80, by = 2))

# impute the Age column:
attach(df.train)
names(df.train)
 
impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 37
        
      }else if (class[i] == 2){
        out[i] <- 29
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
   return(out)
}
 
imputed.age <- impute_age(Age,Pclass)
df.train$Age <- imputed.age

# Now let's check to see if it worked:
missmap(df.train, main="Titanic Training Data Missings Map", col=c("yellow", "black"), legend=FALSE)


#### Building a Logistic Regression Model
## select the relevant columns for training:
df.train <- df.train %>% select(-PassengerId,-Name,-Ticket,-Cabin)
## fit the logistic regression model
log.model <- glm(Survived ~ . , family = binomial(link='logit'), data = df.train)
summary(log.model)
# We can see clearly that Sex, Age, and Class are the most significant features. 
# Which makes sense given the women and children first policy.

#### Prediction:
library(caTools)
set.seed(101)
split = sample.split(df.train$Survived, SplitRatio = 0.70)
final.train = subset(df.train, split == TRUE)
final.test = subset(df.train, split == FALSE)

final.log.model <- glm(Survived ~ . , family = binomial(link='logit'), data = final.train)
summary(final.log.model)

# compute survival probability for each row in the test dataset:
fitted.probabilities <- predict(final.log.model, newdata=final.test, type='response')
# class(fitted.probabilities)
# summary(fitted.probabilities)

# now convert the fitted.probabilities into 1's and 0's:
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)

# missclassification rate:
missclass.rate <- mean(fitted.results != final.test$Survived)

# accuracy rate
accuracy.rate <- 1-missclass.rate
 
# confusion matrix:
table <- table(final.test$Survived, fitted.probabilities > 0.5) # table(Y=1, Y_hat=1)
colnames(table) <- c('computed Not Survived', 'computed Survived')
rownames(table) <- c('Not Survived', 'Survived')
table





 

