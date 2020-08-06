#Michael Erwin
#Exercise 5

#Read in the file
data = read.csv("Album_Sales_3.csv",header=T,sep=',')

attach(data)

#Uncomment which one to plot
#plot(data[,c("adverts","sales")])
#plot(data[,c("airplay","sales")])
plot(data[,c("attract","sales")])

#linear regression based on sales (outcome) and adverts (predictor)
model_one = lm(sales ~ adverts, data=data)

#info for model one
summary(model_one)
print(model_one)

#logistic model with all the variables as predictors 
model_two = lm(sales ~ adverts + airplay + attract, data=data)

#info for model two
summary(model_two)
print(model_two)

#anova function to compare models
anova(model_one, model_two)

