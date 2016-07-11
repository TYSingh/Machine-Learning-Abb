library(ISLR)
library(FNN)
?Caravan
data <- Caravan
head(data)
summary(Caravan$Purchase)

# KNN algorithm need to have all predictor variables to be numeric since we need to compute the distance
# between observations

summary(data$Purchase)
# We need to make sure that our data is standardized

# the first two variables have very different ranges, one that proves why need to standardize
var(data[,1])
var(data[,2])

# 
std_data = scale(data[-86])        # column 86 is categorical variable i.e. Purchase
Purchase = data[,86]

# the following shows that all variables are now on the same scale,so we are safe to run KNN
var(std_data[,1])
var(std_data[,2])

# Now it is time to built KNN model, Split the data into training and testing

set.seed(1)
test = sample(1:5822,1000)
train = -test

training_data = std_data[train,]
testing_data = std_data[test,]

testing_y = Purchase[test]
training_y = Purchase[train]

# Run KNN with K=1

# knn() has sme randomness in it, so we need to set a seed in order to get similar results

set.seed(1)
# it takes the training data and training y and use it on testing data
predicted_y = knn(training_data,testing_data,training_y,k=1) 
head(predicted_y)

# Compute the MSE
mean(predicted_y != testing_y)   # Misclassification error is 10.4,Accuracy is 89.6

# Confusion Matrix
table(testing_y,predicted_y)

# Run KNN for k=3
set.seed(1)
predicted_y = knn(training_data,testing_data,training_y,k=3) 
head(predicted_y)

# Compute the MSE
mean(predicted_y != testing_y)   # Misclassification error is 7.6,Accuracy is 93.4, it doesn,t mean it is 
#the best model


# Confusion Matrix
table(testing_y,predicted_y)

# According to the Confusion matrix only two people will end up buying our policy, and this is not what we 
#prefer. in the previous model where k=1, we were able to have 6 people who bought this policy

# Iterate for different values of k

MSE = NULL
for(i in 1:15) {
  
  set.seed(1)
  predicted_y = knn(training_data,testing_data,training_y,k=i)
  
  MSE[i] = mean(predicted_y != testing_y)
}
  
MSE

# It seems that when k=8 and k=14 we are getting the lowest MSE. We can use KNN with K=8, because this will
# be a simpler model

set.seed(1)
predicted_y = knn(training_data,testing_data,training_y,k=8) 
table(testing_y,predicted_y)

# We can run for loops for different values of k
MSE = NULL
for(i in 1:15) {
  
  set.seed(1)
  predicted_y = knn(training_data,testing_data,training_y,k=i)
  
  MSE[i] = mean(predicted_y != testing_y)
  print(i)
  print(table(testing_y,predicted_y))
}

library(caret)
confusionMatrix(testing_y,predicted_y) 
