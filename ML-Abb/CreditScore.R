
#We would like to predict the Delinquency of bank loans using Credit Scoring 
# A high rate of default is undesirable for a bank because it means that the bank is
# unlikely to fully recover its investment. If we are successful, our model will identify
# applicants that are likely to default, so that this number can be reduced.
# Data has been taken from Kaggle credit dataset

creditdata <- read.csv (file.choose(), header = T)
head(creditdata)

#We will use the training data for training the model and testing data to validate the model. 70 percent of the dataset will be
# used for training the model and 30 percent of the dataset will be used for testing.

creditscoretrain <- creditdata[1:105000,]
head(creditscoretrain)
nrow(creditscoretrain)

creditscoretest <- creditdata[105001:150000,]
head(creditscoretest)
nrow(creditscoretest)

attach(creditscoretrain)
attach(creditscoretest)

Delinquency <- creditscoretrain$Delinquency
Delinquency
Delinquency <- as.factor(Delinquency)
class(Delinquency)

#Logistic Regression model
model = glm(Delinquency ~., data = creditscoretrain ,family = "binomial")
summary(model)

# The significant variables are Age, Time3059D,MonInc,Time90D,RELoans,Time6089D,Dependants. The other variables like RevoUnsec,DebtRatio,CredLine
# are not significant, so we will remove these variables from our model

model1 =  glm(Delinquency ~ Age + Time3059D + MonInc + Time90D + RELoans + Time6089D + Dependants, data = creditscoretrain ,family = "binomial")
summary(model1)

# We will check the significance level using each variable
modelA = glm(Delinquency ~ Age, data = creditscoretrain ,family = "binomial")
summary(modelA)

modelT = glm(Delinquency ~ Time3059D, data = creditscoretrain ,family = "binomial")
summary(modelT)

modelM = glm(Delinquency ~ MonInc, data = creditscoretrain ,family = "binomial")
summary(modelM)

modelT9 = glm(Delinquency ~ Time90D, data = creditscoretrain ,family = "binomial")
summary(modelT9)

modelR = glm(Delinquency ~ RELoans, data = creditscoretrain ,family = "binomial")
summary(modelR)

modelT6 = glm(Delinquency ~ Time6089D, data = creditscoretrain ,family = "binomial")
summary(modelT6)

modelD = glm(Delinquency ~ Dependants, data = creditscoretrain ,family = "binomial")
summary(modelD)



# Visualize the coefficients to see that how far they are from zero
library(coefplot)
coefplot(model1)

# Zoom into the estimates of the variables without the intercept

coefplot(model1, coefficients =c("Age","Time3059D","MonInc","Time90D","RELoans","Time6089D","Dependants"))
coefplot(model, coefficients =c("RevoUnsec","Age","Time3059D","DebtRatio", "MonInc","CredLine","Time90D","RELoans","Time6089D","Dependants"))


# Assess Model1

predicted_score = predict(model1,creditscoretest,type = "response")
head(predicted_score)

testing_score = creditscoretest$Delinquency
testing_score

#Create categorical variable using the predicted probabilities
predicted_score_cat = rep(0,45000)
predicted_score_cat[predicted_score > 0.5] = 1


#Create the Confusion matrix
table(testing_score,predicted_score_cat)

#Accuracy
mean(predicted_score_cat == testing_score)

# Our accuracy rate for our model is 0.9335 which signify that all the variables that are considered are good enough for calculating the score

# Misclassification Error
mean(predicted_score_cat != testing_score)

# Our misclassification rate for our model is 0.06 which signify that our error rate is quite low

#Calculating ROC curve for model

library(ROCR)
pred <- prediction(testing_score,predicted_score_cat)
perf <- performance(pred,"tpr","fpr")
plot(perf,col = "pink",main = "ROC curve for Credit Scoring")
abline(a=0,b=1,lwd=2,lty=2,col ="light blue")

# We can see the performance of the model rises well above the diagonal line, which indicates that the model performance is good

# Calculating KS statistic used to judge model performance
ksstatistic <- max(attr(perf,"y.values")[[1]]- (attr(perf,"x.values")[[1]]))
ksstatistic
