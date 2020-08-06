#Michael Erwin
#Data Analytics
#Exercise 13

#View the dataset
mtcars

#We only want mpg, seconds to go a quarter of a mile, and horsepower
cars = mtcars[,c("mpg", "qsec", "hp")]

#View cars
cars

#Load the package
library(mvoutlier)

#To view a graph of the outliers. Then on the console is a list of the 
#items with a value of 'true' or 'false' referring to whether or 
#not they are outliers.
uni.plot(cars)

#Outliers are:
#Merc 230, Fiat 128, Maserati Bora, and the Toyota Corolla

#Next problem:

#Load bank data
bankData = read.csv('bankloan.csv', header = T, sep = ',')

#View top of bank data
head(bankData)

#Let's choose which columns we care about
cols = c(1,5,6,7,11,13,14)

#Boxplot of selected data
boxplot(bankData[,cols])

#Boxplot of just column 1
b1 = boxplot(bankData[,1])

#Which values are outliers in column 1
which(bankData[,1] %in% b1$out)

#Boxplot of just column 5
b5 = boxplot(bankData[,5])

#Which values are outliers in column 5
which(bankData[,5] %in% b5$out)

#Boxplot of just column 6
b6 = boxplot(bankData[,6])

#Which values are outliers in column 6
which(bankData[,6] %in% b6$out)

#Boxplot of just column 7
b7 = boxplot(bankData[,7])

#Which values are outliers in column 7
which(bankData[,7] %in% b7$out)

#Boxplot of just column 11
b11 = boxplot(bankData[,11])

#Which values are outliers in column 11
which(bankData[,11] %in% b11$out)

#Boxplot of just column 13
b13 = boxplot(bankData[,13])

#Which values are outliers in column 13
which(bankData[,13] %in% b13$out)

#Boxplot of just column 14
b14 = boxplot(bankData[,14])

#Which values are outliers in column 14
which(bankData[,14] %in% b14$out)

#However, we want to detect outliers using the attributes collectively.

#Load the package
library(cluster)

#Create the distance (similarity) function
dist = daisy(bankData[,-19], stand=T, metric=c("gower"), type=list(nominal=c(2,4,12), symm=c(3), asymm=c(10,15,16)))

#Load the package
library(DMwR)

#Get the outliers based on the distance function
out = outliers.ranking(dist, method = "sizeDiff", meth = "ward.D")

#Print the top 10 outliers
print(out$rank.outliers, max = 10)

