#Jorge Moscat Mini-project

# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle","NbClust"))
install.packages("cluster")
library("cluster")


# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
wine_reduced <- wine %>% select(-Type)
wine_scaled <- scale(wine_reduced, center = TRUE)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine_scaled)

# Exercise 2:
#   * How many clusters does this method suggest?
#     --> Based on the plot, it seems that the optimal number of clusters are 3-4
#   * Why does this method work? What's the intuition behind it
#   * Look at the code for wssplot() and figure out how it works
#     --> This method runs the k-means algorithm for k=0 up to 14 and calculates the 
#         WCSS for each of the runs. With this info it is easier to determine the optimal number of clusters that 
#         number of clusters that guarantee the best compactness of clusters

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine_scaled, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
#3 clusters

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(wine_scaled, centers=3 )

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

check.km <- table(wine$Type,fit.km$cluster )
#The clustering model using k=3 can be considered a pretty accurate model. When compared
# with the actual wine types, only 6 instances out of 178 were not correctly classified; which means
# the accuracy of the model is close to ~96%

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

clusplot(wine_reduced,fit.km$cluster)


