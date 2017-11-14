####################################################
#### Unsupervised Learning - K Means Clustering ####
####################################################

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



####
## Get The Data 
####

library(datasets)
head(iris)



####
## Exploratory Data Analysis (EDA)
####
library(tidyverse)

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
# Note that we can put color = Species inside the aes() directly instead of putting it inside geom_point()



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


# Luckily we already know how many clusters to expect
irisCluster <- kmeans(iris[, 1:4], 3, nstart = 20)
irisCluster

# names we can call of the model:
names(irisCluster)
# "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
# "betweenss"    "size"         "iter"         "ifault" 

irisCluster$cluster
# A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
  
# confusion table 
table(iris$Species, irisCluster$cluster)



####
## Cluster Visualizations
####
# Draws a 2-dimensional “clusplot” on the current graphics device.

library(cluster)
clusplot(iris, irisCluster$cluster, color=TRUE, shade=TRUE, labels=0, lines=0)


# read this documentation.
help(clusplot) 



