#Ques.1
data <- read.csv (file.choose(), header = T)  
data
head(data)
attach(data)


#Ques.2
set.seed(100)
train = sample(1:1241,1000)   # take the vector and no. of observations from the sample
head(train)
test = - train
data_train <- data[train,-1 ]                   
dim(data_train)
data_test <- data[test,-1 ]
dim(data_test)


#Ques.3
model = lm(log(Price) ~. , data = data_train)
summary(model)
##Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 1.437899   0.058869  24.425  < 2e-16 ***
#Food        0.015906   0.003699   4.301 1.87e-05 ***
#Decor       0.055666   0.002260  24.627  < 2e-16 ***
#Service     0.038875   0.004402   8.831  < 2e-16 ***

#Residual standard error: 0.2592 on 996 degrees of freedom
#Multiple R-squared:  0.6793,	Adjusted R-squared:  0.6784 
#F-statistic: 703.4 on 3 and 996 DF,  p-value: < 2.2e-16


#Ans.3 Food ,Decor & Service are statistically significant as P value is less than 0.05 
#R square value is 0.6793 

# We have used log of Price with which our R square has increased


##Assess the model using testing data
testing_y = data_test$Price
testing_y

## Predict the predicted y
predicted_y = predict(model,data_test)
testing_y
predicted_y
error = testing_y - exp(predicted_y)
error_squared = error^2
MSE = mean(error_squared)
MSE                 


# Ques.4
##There are many ways to check collinearity
#1. Create the correlation matrix
#2. Look at the correlation plot
#3. Look at the VIF

#correlations
round(cor(data_train),2)

#visualize the correlations
library(corrplot)
corr_matrix = cor(data_train)
corrplot(corr_matrix, order = "hclust")

# VIF                  # If packages are lower then use install.packages("car",dependencies = T)
library(car)
vif(model)            # Higher the number, the more it is affecting the collinearity e.g. tax

## According to VIF output, we can see Service is affecting the collinearity in the model.
## This is because they have a high VIF(larger than 5) , VIF = 1/(1-R^2)

## We need to get rid of either rad or tax

model = lm(log(Price) ~. -Food, data = data_train)
summary(model)

#Multiple R-squared:  0.6734,	Adjusted R-squared:  0.6727 


model = lm(log(Price) ~. -Decor, data = data_train)
summary(model)

#Multiple R-squared:  0.4841,	Adjusted R-squared:  0.4831 



model = lm(log(Price) ~. -Service, data = data_train)
summary(model)

#Multiple R-squared:  0.6542,	Adjusted R-squared:  0.6535 

