## Load the MASS library
library(MASS)
data <- Boston
?Boston

##Simple Linear Regression
##Predict the medv from lstat

##Split the data into training and testing
##Randomly select 400 observations for training and rest for testing data
set.seed(100)
train = sample(1:506,400)   # take the vector and no. of observations from the sample
head(train)
test = - train
training_data = Boston[train,c("lstat","medv")]
testing_data = Boston[test,c("lstat","medv")]
View(training_data)

##Linearity of the model
##We tried different transformations and taking the log of both x and y increases the linearity of the model
plot(training_data$lstat,training_data$medv)
plot(log(training_data$lstat),log(training_data$medv), xlab ="log lstat",ylab="log medv")

##Create our Linear Model
model = lm(log(medv) ~ log(lstat), data= training_data)
summary(model)

## The log(lstat) has negative coefficient which means for every one unit increase in log(lstat) , there will be 
## 0.56121 decrease in log(medv)

##R squared=0.6608, this means 66.83% of the variation in the model is explained by variable log(lstat)

## The model as a whole is significant since the F-value is very high

##The following model has less R squared has compared to model1, So we'll take model1
model0 = lm(medv ~ lstat, data= training_data)
summary(model0)
plot(log(training_data$lstat),log(training_data$medv), xlab ="log lstat",ylab="log medv")
abline(model, col = "pink" , lwd = 2)

##Assess the model using testing data
## error term will be testing data - predicted data
testing_y = testing_data$medv
testing_y
predicted_y = predict(model, data.frame(lstat  = testing_data$lstat))
predicted_y

##Compute the MSE
error = testing_y - exp(predicted_y)
error_squared = error^2
MSE = mean(error_squared)
MSE


##Multiple Linear Regression
##Split the data into testing and training
set.seed(100)
train = sample(1:506,400)
test = -train

training_data = Boston[train,]
testing_data = Boston[test,]
?Boston

##run a linear regression model to predict medv from lstat and rm

##Check the linearity between medv and rm

##trying different combinations of transformations,we don't get any improvement, so we will not do any transformation
plot(training_data$rm,training_data$medv)

##Train the model using training data, we'll be using the second model as adjusted R squared is higher
model = lm(medv~ lstat + rm, data = training_data)
summary(model)
model = lm(medv~ log(lstat) + rm, data = training_data)
summary(model)
model = lm(log(medv)~ log(lstat) + rm, data = training_data)
summary(model)

##Assess the model using testing data
testing_y = testing_data$medv

## Predict the predicted y
predicted_y = predict(model,testing_data)
testing_y
predicted_y
error = testing_y - (predicted_y)
error_squared = error^2
MSE = mean(error_squared)
MSE                  # give 

##Running MLR using all predictors

##Split the data into testing and training
set.seed(100)
train = sample(1:506,400)
test = -train

training_data = Boston[train,]
testing_data = Boston[test,]

##Train the models using all predictors
model = lm(medv ~ ., data = training_data)
summary(model)

##We wanted to use log(lstat) and not lstat
model = lm(medv ~ ., + log(lstat) - lstat,data = training_data)
summary(model)

##There are many ways to check collinearity
#1. Create the correlation matrix
#2. Look at the correlation plot
#3. Look at the VIF

#correlations
round(cor(training_data),2)

#visualize the correlations
library(corrplot)
corr_matrix = cor(training_data)
corrplot(corr_matrix, order = "hclust")

# VIF                  # If packages are lower then use install.packages("car",dependencies = T)
library(car)
vif(model)            # Higher the number, the more it is affecting the collinearity e.g. tax

## According to VIF output, we can see the tax and rad are affecting the collinearity in the model.
## This is because they have a high VIF(larger than 5) , VIF = 1/(1-R^2)

## We need to get rid of either rad or tax

model = lm(medv ~ ., + log(lstat) - lstat - tax,data = training_data)
summary(model)

## We tried to get rid of rad, and the R squared was lower when we got rid of tax

##Assessing the model using the testing data
testing_y = testing_data$medv

## Predict the predicted y
predicted_y = predict(model,testing_data)
testing_y
predicted_y
error = testing_y - (predicted_y)
error_squared = error^2
MSE = mean(error_squared)
MSE          

## Dealing with interaction terms

model = lm(medv ~ log(lstat) * rm,data = training_data) # it makes sense to havde interaction term as it increases the adjusted R squared
summary(model)

##Get all two way interaction term
model = lm(medv ~ (.)^3, data = training_data)
summary(model)

##Dealing with Categorical Variables
library(ISLR)
?Carseats
attach(Carseats)

## Split the data
set.seed(1)
train = sample(1:400,300)
test = - train

training_data = Carseats[train,]
testing_data = Carseats[test,]

##Training the model
model = lm(Sales ~ Price + ShelveLoc, dat = training_data )
summary(model)

## When we have a categorical variables, R creates a dummy variable
contrasts(Carseats$ShelveLoc)


##Second Session
library(ISLR)
?Hitters

## check for missing values 
sum(is.na(Hitters))

##get rid of the missing observations with missing values
Hitters <- na.omit(Hitters)
Hitters


##load the package leaps
library(leaps)
## use the leaps package to run subset selection using regsub()
model = regsubsets(Salary ~., data = Hitters,nvmax=19)
model
model_summary =summary(model)
names(model_summary)

## We would like to make a decision on how many variables to include in the model based on adjusted R square,Cp,BIC,tec.

par(mfrow=c(2,2))
# R-square
plot(1:19,model_summary$rsq, xlab ="Number of Predictors",ylab = "R-Square Value", type = "l")
axis(side = 1, at = 1:19, labels = 1:19)

index = which.max(model_summary$rsq)
index
abline(v = index,col = "green",lwd=2)
points(index, max(model_summary$rsq),col = "red",pch=20)

#Adjusted R square

plot(1:19,model_summary$adjr2, xlab ="Number of Predictors",ylab = "Adjusted R-Square Value", type = "l")
axis(side = 1, at = 1:19, labels = 1:19)

index = which.max(model_summary$adjr2)
index
abline(v = index,col = "green",lwd=2)
points(index, max(model_summary$adjr2),col = "red",pch=20)

#BIC

plot(1:19,model_summary$bic, xlab ="Number of Predictors",ylab = "BIC Value", type = "l")
axis(side = 1, at = 1:19, labels = 1:19)

index = which.min(model_summary$bic)
index
abline(v = index,col = "green",lwd=2)
points(index, min(model_summary$bic),col = "red",pch=20)

#CP
plot(1:19,model_summary$cp, xlab ="Number of Predictors",ylab = "CP Value", type = "l")
axis(side = 1, at = 1:19, labels = 1:19)

index = which.min(model_summary$cp)
index
abline(v = index,col = "green",lwd=2)
points(index, min(model_summary$cp),col = "red",pch=20)
# through this we interpret that the no. of predictors should be 6 (predictors should be less in value
#but at the same time R square & adjusted r square should be more), so nvmax should be set 6
