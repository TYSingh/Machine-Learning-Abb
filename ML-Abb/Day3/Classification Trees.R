# Classification Trees
library(ISLR)
library(tree)

# Use the Carseat dataset from the ISLR package
attach(Carseats)
?Carseats

# The sales variable is numeric. I will categorise is to see is the sales were high(Sales >=8) or Low (Sales<8)

data <- Carseats
data$High = ifelse(data$Sales >=8, "Yes","No")

# Split the data

set.seed(1)
train = sample(1:nrow(data),200)
test = -train

training_data = data[train,]
testing_data = data[test,]

# Fit the decision tree model for classification

model = tree(as.factor(High)~. - Sales, training_data)
plot(model)
text(model, pretty = 0)

# check if we need to prune the tree
set.seed(1)
validated_tree = cv.tree(model,FUN  = prune.misclass)
validated_tree
which.min(validated_tree$dev)
validated_tree$size

#Asess the model

predicted_prob = predict(model,testing_data)
predicted_y = rep("High",200)
predicted_y[predicted_prob < 0.5] = "No"
length(predicted_y)
testing_y = data$High[test]

mean((as.factor(predicted_y) - testing_y)^2)
