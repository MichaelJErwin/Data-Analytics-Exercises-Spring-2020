#Michael Erwin
#Data Analytics
#Exercise 11

#Read in the data
data = read.csv('adultsData.csv', sep=',', header=T)

#Create the sample
test_sample = floor(0.2*nrow(data))
test_sample = sample(seq_len(nrow(data)), size=test_sample)

#Subset
test_data = data[test_sample, ]
train_data = data[-test_sample, ]

#We can see that the verification data is only 20% of the size of the original data
dim(test_data)[1] / dim(data)[1]

#Load the library
library(rpart)

#Create the decision tree using the training data 
tree = rpart(class ~ ., data = train_data)

#Load the library
library(rpart.plot)

#Plot the Decision Tree
rpart.plot(tree)

#Let's try the model against the test data to see how it does 
tree.predicted = predict(tree, test_data, type = "class")

#Load the libraries
library(e1071)
library(caret)

#Display the accuracy 
confusionMatrix(tree.predicted, test_data$class)

#The accuracy I'm getting is around 84-85%

#Confusion matrix
table(tree.predicted, test_data$class)

#Load the library
library(naivebayes)

#Build naive Bayesian model on training data
nmodel = naive_bayes(class ~ ., data = train_data)

#The model is now trained. Let's test against test data
prediction = predict(nmodel, test_data)

#Display the accuracy 
confusionMatrix(prediction, test_data$class)

#The accuracy I'm getting is around 83%

#Confusion matrix
table(prediction, test_data$class)



