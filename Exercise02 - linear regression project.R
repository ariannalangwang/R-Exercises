###################################
#### LINEAR REGRESSION PROJECT ####
###################################
# The main point of this project is to get you feeling comfortabe with Exploratory Data Analysis 
# and begin to get an understanding that sometimes certain models are not a good choice for a data set. 
# In this case, we will discover that Linear Regression may not be the best choice given our data!

library(tidyverse)
bike <- read.csv('bikeshare.csv')  
head(bike)
str(bike)
summary(bike)
# Count is what we are trying to predict.

###
####Data Cleaning & Feature Engineer
###
any(is.na(bike))  # FALSE

# Create an "hour" column that takes the hour from the datetime column:
bike$hour <- sapply(bike$datetime,function(x){format(x,"%H")})

# Change column formats:
bike$season <- sapply(bike$season, factor)
bike$holiday <- sapply(bike$holiday, factor)
bike$workingday <- sapply(bike$workingday, factor)
bike$weather <- sapply(bike$weather, factor)
bike$hour <- sapply(bike$hour, as.numeric)

###
#### Exploratory Data Analysis (EDA)
###
# What is the correlation between temp and count?
cor(bike$temp, bike$count)
# or:
cor(bike[ ,c('temp','count')])

# Explore seasonality. 
# Create a boxplot, with the y axis indicating count and the x axis for each season.
ggplot(bike,aes(season,count)) + geom_boxplot(aes(color=season)) +theme_bw()

# Plot count vs. temp
ggplot(bike,aes(temp,count)) + geom_point(alpha=0.2, color='light blue') + theme_bw()

# Plot count versus datetime as a scatterplot with a color gradient based on temperature. 
# You'll need to convert the datetime column into POSIXct before plotting.
bike$datetime <- as.POSIXct(bike$datetime)
ggplot(bike,aes(datetime,count)) + geom_point(aes(color=temp),alpha=0.5)  
      + scale_color_continuous(low='#55D8CE',high='#FF6E2E') +theme_bw()
# Notice two things: A seasonality to the data, for winter and summer. 
# Also, bike rental counts are increasing in general. 
# So, not a good idea to use a linear regression model.

# create a scatterplot of count versus hour, with color scale based on temp. 
# Only use bike data where workingday==1.
ggplot(filter(bike,workingday==1), aes(hour,count)) 
      + geom_point(position=position_jitter(w=1, h=0), aes(color=temp), alpha=0.5) 
      + scale_color_gradientn(colors = c('dark blue','blue','light blue','light green','yellow','orange','red'))
      + theme_bw()

# Now create the same plot for non working days:
ggplot(filter(bike,workingday==0), aes(hour,count)) 
      + geom_point(position=position_jitter(w=1, h=0), aes(color=temp), alpha=0.5) 
      + scale_color_gradientn(colors = c('dark blue','blue','light blue','light green','yellow','orange','red'))
      + theme_bw()

####
#### Building the Model
# Use lm() build a model that predicts count based solely on temp:
temp.model <- lm(count~temp,bike)
summary(temp.model)

####
#### Predictions
####
# How many bike rentals would we predict if the temperature was 25 degrees Celsius?
temp25 <- data.frame(temp=c(25))
predict(temp.model,newdata=temp25)

# Multivariate Linear Model:
model <- lm(count ~ . -casual - registered -datetime -atemp,bike )
summary(model)

# You should have noticed that this sort of model doesn't work well given our 
# seasonal and time series data. We need a model that can account for this type 
# of trend, read about Regression Forests for more info if you're interested!





















