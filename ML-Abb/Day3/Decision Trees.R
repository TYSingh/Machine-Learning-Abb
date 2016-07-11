# Regression Trees
library(tree)
library(MASS)
attach(Boston)
# We will use the Boston dataset to predict the median value of the house
# Split the data into training and testing

set.seed(1)
train = sample(1:nrow(Boston),400)
test = - train

training_data = Boston[train,]
testing_data = Boston[test,]

# Train the model using the training dataset
model= tree(medv ~.,data=training_data)
model
plot(model)
text(model)

# Check if the tree needs pruning
validated_tree = cv.tree(model)
validated_tree
plot(validated_tree$size,validated_tree$dev,type = "b",ylab = "RSS", xlab ="Size of the tree")

# According to the plot the size of the tree should be 9, so no need to prune the tree

# Assess the model and find out what is the MSE
predicted_y = predict(model,testing_data)
testing_y = Boston$medv[test]

MSE = mean((predicted_y - testing_y)^2)
MSE

# Let's assume that we need to prune our tree to size=7(eventhough we don't need to prune it, but this 
# is just the case we need to prune)

pruned_tree = prune.tree(model, best = 7)
plot(pruned_tree)
text(pruned_tree)

predicted_y = predict(pruned_tree, testing_data)
testing_y = Boston$medv[test]
MSE = mean((predicted_y - testing_y)^2)
MSE

# The MSE is 13.04 which is more compared to when we did not prune the tree. this proves pruning the tree 
# won't work here