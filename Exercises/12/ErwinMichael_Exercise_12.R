#Michael Erwin
#Data Analytics
#Exercise 12

#Read in the data
data = read.csv('PotentialClients.csv', header = T, sep = ',')

#Scatter plot of the data
plot(data)

#From a quick observation of the scatter plot, we can see that 
#there are a maximum of 15 groups of data points.
k.max = 15

#Compute WSS (weighted sum statistic) for k = 2 to k = 15
wss = sapply(1:k.max, function(k) {kmeans(data, k, nstart=50, iter.max=15)$tot.withinss})

#Plot WSS 
plot(1:k.max, wss, type="b", pch=19, frame=F, xlab="Number of clusters K", ylab = "Total within-clusters sum of square")

#Let's say 6 was the optimum number of clusters

clusters.km = kmeans(data,6)

#Now to plot clusters (color-coded)
plot(data, col=clusters.km$cluster)

library(mclust)

#Alternatively, use Bayesian Information Criterion to determine k amount of clusters
d_clust = Mclust(as.matrix(data), G=1:15, modelNames=mclust.options("emModelNames"))

#Plot. Choose 1 in the console when prompted. (The BIC option)
plot(d_clust)

#The lower the BIC value, the better. This methods tells us that k = 15 is the 
#optimum amount of clusters.

clusters.km = kmeans(data,15)

#Now to plot clusters (color-coded)
plot(data, col=clusters.km$cluster)

#This may be too many, however. When plotted, the clusters probably aren't grouped
#how you might think they should be. For example, a grouping of data points 
#(which should probably be one cluster) might constitute two or three clusters. 
#It might be better to decide the proper cluster amount based on common sense and 
#observation of the data.

