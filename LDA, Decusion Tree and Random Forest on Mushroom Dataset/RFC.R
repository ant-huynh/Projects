#Import libraries
library(tidyverse)
library(randomForest)
library(rpart)

#Load the data
setwd("C:/Users/Yadu/Desktop/BH119/Sem 2-2020/Predictive Modelling/Project")
mushroom <- read.csv("mushrooms.csv")

#Remove veil.type
mushroom$veil.type <-NULL

#Convert all variables into factors
mushroom <- mushroom %>% map_df(function(.x) as.factor(.x))

#Split the data into training and testing datasets
set.seed(10)
mushsample <- caret::createDataPartition(y = mushroom$class, times = 1, p = 0.8, list = FALSE)
train_mushroom <- mushroom[mushsample, ]
test_mushroom <- mushroom[-mushsample, ]

#Fit a random forest classifier to the training data
model_rf <- randomForest(class ~ ., ntree = 50, data = train_mushroom)
plot(model_rf, main='Random Forest Classifier', col.main='darkblue')

#Confusion matrix and Out-of-bag error rate
print(model_rf)

#Plot indicating the top 10 variables that had the greatest impact in the rf model
varImpPlot(model_rf, sort = TRUE, 
           n.var = 10, main = "The 10 variables with the most predictive power", col.main='darkblue')

#Use the fitted model to predict the test data
model_rf.predict = predict(model_rf , test_mushroom)

#Confusion matrix
caret::confusionMatrix(data = model_rf.predict,reference = test_mushroom$class,positive = 'e')
