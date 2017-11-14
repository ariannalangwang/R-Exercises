############################################################
#### Unsupervised Learning - K Means Clustering Project ####
############################################################

# Methodology:

# K Means Clustering is an unsupervised learning algorithm that tries to cluster data based on their similarity. 
# Unsupervised learning means that there is no outcome to be predicted, and the algorithm just tries to find patterns in the data. 
# In k means clustering, we have to specify the number of clusters we want the data to be grouped into (k = ). 

# The algorithm randomly assigns each observation to a cluster, and finds the centroid of each cluster. 
# Then, the algorithm iterates through two steps:
#   1. Reassign data points to the cluster whose centroid is closest. 
#   2. Calculate new centroid of each cluster. 
# These two steps are repeated till the within cluster variation cannot be reduced any further. 
# The within cluster variation is calculated as the sum of the euclidean distance between the data points 
# and their respective cluster centroids.


# The Project:
# Usually when dealing with an unsupervised learning problem, 
# it's difficult to get a good measure of how well the model performed. 
# For this project, we will use data from the UCI archive based off of red and white wines.



####
## Get The Data 
####
library(tidyverse)

# Use read.csv to open both data sets and set them as df1 and df2. 
# Pay attention to what the separator (sep) is.
df1 <- read.csv('winequality-red.csv', sep=';')
df2 <- read.csv('winequality-white.csv', sep=';')

# Now add a label column to both df1 and df2 indicating a label 'red' or 'white'.
df1$label <- 'red'
df2$label <- 'white'

# Combine df1 and df2 into a single data frame called wine.
wine <- rbind(df1, df2)

str(wine)



####
## Exploratory Data Analysis (EDA)
####

# Create a Histogram of residual sugar from the wine data. Color by red and white wines.
ggplot(data=wine, aes(x=residual.sugar)) + geom_histogram(aes(fill=label), color='black', bins=50) + 
       scale_fill_manual(values = c('red','#faf7ea')) + theme_bw()
       # adding fill colors

# Create a Histogram of citric.acid from the wine data. Color by red and white wines.
ggplot(data=wine, aes(x=citric.acid)) + geom_histogram(aes(fill=label), color='black', bins=50) + 
       scale_fill_manual(values = c('red','#faf7ea')) + theme_bw()

# Create a Histogram of alcohol from the wine data. Color by red and white wines.
ggplot(data=wine, aes(x=alcohol)) + geom_histogram(aes(fill=label), color='black', bins=50) + 
       scale_fill_manual(values = c('red','#faf7ea')) + theme_bw()

# Create a scatterplot of residual.sugar versus citric.acid, color by red and white wine.
ggplot(data=wine, aes(x=citric.acid, y=residual.sugar)) + geom_point(aes(color=label), alpha=0.4) + 
       scale_color_manual(values = c('red','#faf7ea')) + theme_dark()

# Create a scatterplot of residual.sugar versus volatile.acidity, color by red and white wine.
ggplot(data=wine, aes(x=volatile.acidity, y=residual.sugar)) + geom_point(aes(color=label), alpha=0.4) + 
  scale_color_manual(values = c('red','#faf7ea')) + theme_dark()

# Grab the wine data without the label and call it clus.data
clus.data <- subset(wine, select=c(-label))

# Check the head of clus.data
head(clus.data)




####
## Build the Model
####

# Now let's attempt to use the K-means algorithm to cluster the data. 
# Remember that this is an unsupervised learning algorithm, 
# meaning we won't give any information on the correct labels.


help(kmeans)
# Perform k-means clustering on a data matrix.

# kmeans(x, centers, iter.max = 10, nstart = 1,
#        algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), 
#        trace=FALSE)


# Call the kmeans function on clus.data and assign the results to wine.cluster.
wine.cluster <- kmeans(clus.data, 2)


# What functions can we call on the kmeans function?
names(wine.cluster)
# "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"   
# "size"         "iter"         "ifault"   


# Print out the wine.cluster Cluster Means
wine.cluster$centers




####
## Evaluate the Clusters
####

# You usually won't have the luxury of labeled data with KMeans, but let's go ahead and see how we did!
# Use the table() function to compare your cluster results to the real results. 
# Which is easier to correctly group, red or white wines?

table(wine$label, wine.cluster$cluster)      # table(real y, y_hat)

# We can see that red is easier to cluster together, which makes sense given our previous visualizations. 

# It's important to note here, that K-Means can only give you the clusters, it can't directly tell you 
# what the labels should be, or even how many clusters you should have, we are just lucky to know we 
# expected two types of wine. This is where domain knowledge really comes into play.







