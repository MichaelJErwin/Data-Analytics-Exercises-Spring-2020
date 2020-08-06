#Michael Erwin
#Data Analytics
#Exercise 7


#Read in the data
data = read.delim("data.dat", header = T)

#Get covariance matrix, round numbers to 2 decimal places
covariance_matrix = cor(data)

#To better view the data
round(covariance_matrix,2)

#Correlations less than 0.3 might not be meaningful

library(pastecs)

#Now to calculate mean correlation for each IAS
stat.desc(covariance_matrix)

#IAS 13, 22, and 32 have a low mean correlation.
#IAS 23 and 34 have low variance.

library(gdata)

#Remove columns
data.2 = remove.vars(data, c("ias13", "ias22", "ias32", "ias23", "ias34"))

library(psych)

#Principal components
pc1 = principal(data.2, nfactors=31)

#Now to check for eigenvalues less than 1

pc1$values

#It looks like the first 5 values are the most influential. 
#We will create another pca with only 5 factors

pc2 = principal(data.2, nfactors=5)

pc2$values

#To view PCA details
print.psych(pc2, cut = 0.3, sort = T)

#To view a plot of the importance of the factors 
plot(pc2$values, type = "b")

#I removed IAS 13, 22, and 32 for having a low mean correlation and ias23 and 34
#for having a low variance.

length(pc2$values)

#My PCA had 31 factors. However, there were only 5 significant factors. 
#If a component has a low eigenvalue then it is contributing little to the explanation 
#of variances in the variables and may be ignored since it is less important than the 
#components with higher eigenvalues.






