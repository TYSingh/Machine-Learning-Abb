library(ISLR)
??Smarket
attach(ISLR)
# We would like to predict the direction of stock price based on the predictor variable

data <- Smarket
data
 
# Split the data into training and testing data

#Since the data is time series, it does not make sense to randomly split the data. Instead we will 2005,
#as our testing data and the rest as our training data
View(data)
train <- data$Year < 2005
test = !train
test

training_data = data[train,-8]
testing_data = data[test,-8]
nrow(training_data)
nrow(testing_data)
# Train the model using testing and training data from 2001 until 2005(excluded)
model = glm(Direction ~., data = training_data ,family = "binomial")
summary(model)

# The only significant variable is the Year. The lag variables are not significant but lag1, lag2 and volume 
# seems to be okay to keep it in our model (even though they are not stastically significant) and this is because
# we are interested in predictions and not interpretations

# Train the model with only these variables

model1 = glm(Direction ~ Year + Lag1 + Lag2 + Volume, data= training_data,family = "binomial")
summary(model1)

# Visualize the coefficients to see that how far they are from zero
library(coefplot)
coefplot(model1)

# Zoom into the estimates of the variables without the intercept

coefplot(model1, coefficients =c("Year","Lag1","Lag2","Volume"))
coefplot(model, coefficients =c("Year","Lag1","Lag2","Lag3","Lag4","Lag5","Volume"))


#Assess Model1

predicted_y = predict(model1,testing_data,type = "response")
head(predicted_y)

#Create categorical variable using the predicted probabilities
predicted_y_cat = rep("Down",252)
predicted_y_cat[predicted_y > 0.5] = "Up"

testing_y = testing_data$Direction

#Create the Confusion matrix
table(testing_y,predicted_y_cat)

#Accuracy
mean(testing_y == predicted_y_cat)
