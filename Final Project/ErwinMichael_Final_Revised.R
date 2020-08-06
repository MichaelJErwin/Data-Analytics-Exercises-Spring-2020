#Michael Erwin
#Data Analytics
#Final Project

#Load in the data
data = read.csv('10kPatients_some_preprocessed.csv', header=T, sep=',')

#Get total of amount of NA values
sum(is.na(data))

#However, we know that there are also blank values, '?' values, and 'Not Available' values
data = read.csv('10kPatients_some_preprocessed.csv', header=T, sep=',', na.strings = c("", "?", "Not Available", "NA", NA))

#Now to get new sum of 'NA' values
sum(is.na(data))

#Yikes. Let's impute missing/invalid values

#Load in the library
library(VIM)

#Impute missing values using kNN technique
data = kNN(data)

#Calculate total 'NA' values
sum(is.na(data))

#0 NA values

#Let's create a subset of the data using relevant columns
sub_data = data[, c("age", "race", "gender", "admission_type_id", "discharge_disposition_id", "diabetesMed", "readmitted")]

#View the first 6 values
head(sub_data)

#Let's do some visualization

#People who were readmitted vs people who weren't
plot(sub_data$readmitted)

#Now, let's calculate the percentages
#Change "Yes" and "No" factors to numeric for counting. No = 1, Yes = 2
#Column 7 is the readmitted column
sub_data[, 7] <- as.numeric(sub_data[, 7])

head(sub_data)

#Count sums
sum(sub_data$readmitted == '1')
sum(sub_data$readmitted == '2')

#6035 = no ; 3965 = yes

#No percentage:
6035 / (6035 + 3965)

#60.35% of patients were not readmitted

#Yes percentage:
3965 / (6035 + 3965)

#39.65% of patients were readmitted

#Now let's restore the data
sub_data = data[, c("age", "race", "gender", "admission_type_id", "discharge_disposition_id", "diabetesMed", "readmitted")]
head(sub_data)

#Whether the patient was prescribed diabetes medicine or not
plot(sub_data$diabetesMed)

#Now, let's calculate the percentages
#Change "Yes" and "No" factors to numeric for counting. No = 1, Yes = 2
sub_data[, 6] <- as.numeric(sub_data[, 6])

head(sub_data)

#Count sums
sum(sub_data$diabetesMed == '1')
sum(sub_data$diabetesMed == '2')

#2522 = no ; 7478 = yes

#No percentage:
2522 / (2522 + 7478)

#25.22% of patients weren't taking diabetes medicine

#Yes percentage:
7478 / (2522 + 7478)

#74.78% of patients were taking diabetes medicine

#Now let's restore the data
sub_data = data[, c("age", "race", "gender", "admission_type_id", "discharge_disposition_id", "diabetesMed", "readmitted")]
head(sub_data)

#Comparing the races
plot(sub_data$race)


#Comparing the ages
plot(sub_data$age)

#Comparing the gender
plot(sub_data$gender)

#Now, let's calculate the percentages
#Change "Female" and "Male" factors to numeric for counting. Female = 1, Male = 2
sub_data[, 3] <- as.numeric(sub_data[, 3])

head(sub_data)

#Count sums
sum(sub_data$gender == '1')
sum(sub_data$gender == '2')

#5398 = female ; 4602 = male

#Female percentage:
5398 / (5398 + 4602)

#53.98% of patients were female

#Yes percentage:
4602 / (5398 + 4602)

#46.02% of patients were male

#Now let's restore the data
sub_data = data[, c("age", "race", "gender", "admission_type_id", "discharge_disposition_id", "diabetesMed", "readmitted")]
head(sub_data, 100)

#Comparing passing away vs staying alive
plot(sub_data$discharge_disposition_id)

#Now, let's calculate the percentages
#Change factors to numeric for counting. Expired = 17
sub_data[, 5] <- as.numeric(sub_data[, 5])

head(sub_data,100)

#Count sums
sum(sub_data$discharge_disposition_id == '17')

#196 = expired

nrow(sub_data)

#Percent of expired patients:
196/10000

#1.96% of patients expired.

#Now let's restore the data
sub_data = data[, c("age", "race", "gender", "admission_type_id", "discharge_disposition_id", "diabetesMed", "readmitted")]
head(sub_data, 100)

#Now let's do some pattern mining

#Load in the library
library(arules)

#Not readmitted

#Run apriori to find the rules
rules1 = apriori(sub_data, parameter=list(minlen=2, supp=0.005, conf=0.6), appearance=list(rhs=c("readmitted=No"), default="lhs"))

#Then sort
rules1 = sort(rules1, by="lift")

#Then inspect which rules had best lift, support, and confidence
inspect(rules1[1:50])

#Now let's check rules for readmitted patients

#Run apriori to find the rules
rules2 = apriori(sub_data, parameter=list(minlen=2, supp=0.005, conf=0.6), appearance=list(rhs=c("readmitted=Yes"), default="lhs"))

#Then sort
rules2 = sort(rules2, by="lift")

#Then inspect which rules had best lift, support, and confidence
inspect(rules2)

#Now let's check rules for expired patients

#Run apriori to find the rules
rulesExpiry = apriori(sub_data, parameter=list(minlen=2, supp=0.005, conf=0.006), appearance=list(rhs=c("discharge_disposition_id=Expired"), default="lhs"))

#Then sort
rulesExpiry = sort(rulesExpiry, by="lift")

#Then inspect which rules had best lift, support, and confidence
inspect(rulesExpiry)

#Now let's train some models to do some predictions

#We'll split the data into 80% for training the model and 20% for validating the model.

#Calculate the testing sample size
TestingSampleSize = floor (0.2 * nrow(data))

#Sample
TestingSample = sample(seq_len(nrow(data)), size = TestingSampleSize)

#Subsetting training and testing data
TestingData  = data[TestingSample,]
TrainingData = data[-TestingSample,]

#Now let's train a model

#Load the library
library(naivebayes)

#Train the naive Bayes model
nmodel = naive_bayes(readmitted ~ ., data = TrainingData)

#The model is trained, let's use it for prediction
predbayes = predict(nmodel, TestingData)

#Let's compare the prediction to the actual
table(predbayes, TestingData$readmitted)

#Let's try a different model

#Load in the library
library(caret)

#Train the neural net model
neural_model = train(readmitted ~ ., data = TrainingData, method = "mlp", metric = "Accuracy")

#Get predictions
predictions_neural = predict(neural_model, TestingData)

#Compare predictions to actual values
table(predictions_neural, TestingData$readmitted)

#Let's try again with the subset 

#Calculate the testing sample size
TestingSampleSize = floor (0.2 * nrow(sub_data))

#Sample
TestingSample = sample(seq_len(nrow(sub_data)), size = TestingSampleSize)

#Subsetting training and testing data
TestingData  = sub_data[TestingSample,]
TrainingData = sub_data[-TestingSample,]

#Now let's train the model

#Train the neural net model
neural_model = train(readmitted ~ ., data = TrainingData, method = "mlp", metric = "Accuracy")

#Get predictions
predictions_neural = predict(neural_model, TestingData)

#Compare predictions to actual values
table(predictions_neural, TestingData$readmitted)


