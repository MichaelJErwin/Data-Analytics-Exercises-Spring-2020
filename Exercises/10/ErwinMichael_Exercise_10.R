#Michael Erwin
#Data Analytics
#Exercise 10

#Load in the data 
data = read.csv('TitanicData.csv',sep=',',header=T)

#First 6 results
head(data)

library(arules)

#To create a narrowed down list of rules
rules = apriori(data, parameter = list(minlen=2, supp=0.005, conf=0.8),appearance = list(rhs=c("Survived=No", "Survived=Yes"),default="lhs"))

#Then sort by lift
rules = sort(rules, by="lift")

#To view the rules:
inspect(rules)





