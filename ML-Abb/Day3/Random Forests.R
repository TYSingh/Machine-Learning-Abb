library(randomForest)
set.seed(1)
train = sample(1:nrow(Boston),400)
test = - train

training_data = Boston[train,]
testing_data = Boston[test,]
ncol(Boston)
# Fit a bagging Model

bag_model = randomForest(medv~. ,data = training_data,mtry=13, importance=T)
bag_model

# assess the bag model
predicted_y = predict(bag_model,testing_data)
testing_y = Boston$medv[test]

mean((predicted_y - testing_y)^2)

# Fit a random Forest Model

bag_model = randomForest(medv~. ,data = training_data,mtry=4, importance=T)
bag_model

# assess the bag model
predicted_y = predict(bag_model,testing_data)
testing_y = Boston$medv[test]

mean((predicted_y - testing_y)^2)

#Variable Importance
imp = importance(bag_model)[,1]
barplot(sort(imp))
