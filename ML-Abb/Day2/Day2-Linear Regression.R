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
test = -train