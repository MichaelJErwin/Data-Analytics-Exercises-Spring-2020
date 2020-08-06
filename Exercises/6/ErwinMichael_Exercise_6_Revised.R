#Michael Erwin
#Data Analytics
#Exercise 6

library(gmodels)

#Read in the data
data = read.csv('Americandata.csv', header=T, sep=',')

#View the data
data

#Get rid of yes/no column
data = data[ , 2:9]

#View new table
data

#Convert to a matrix
data_matrix = data.matrix(data)

#Contingency table:
CrossTable(data_matrix, chisq=T, expected=T, sresid=T, format="SPSS")

#There are some interesting things to notice. First, we can see that students were his main respondent by far.
#In fact, about 46% of all the responses were students who are unhappy :( 

#Housewives, lawyers, and musicians had a significantly smaller chi-square contribution 
#than the other categories. This is because the expected values from the model were pretty close to the observed values 
#compared to the other categories.

#The chi-squared value is about 936.14, which is a rather high value. This could suggest 
#that this model isn't a good fit for the data. Degrees of freedom are the number of observations that are 
#free to vary when estimating statistical parameters. Here, the degrees of freedom value is 7. The formula for determining the 
#degrees of freedom for a contingency chart is (r-1)(c-1) where r is the number of rows and c is the number of columns. Here, 
#the number of columns is 8 and the number of rows is 2. (8-1)*(2-1) = 7 degrees of freedom. Also, the significance value is 
#extremely small. This might suggest a good fit, but we know that this isn't the case because of the chi-squared value. 
#This could suggest that there is an insufficient amount of data.


#The standardized residual is a measure of how significant the cells are to the chi-squared value.
#The only standardized residuals inside of +- 1.96 are Housewives (yes and no)



