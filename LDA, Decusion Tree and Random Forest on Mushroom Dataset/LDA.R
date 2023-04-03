graphics.off() 
rm(list=ls())  
library(MASS)
library(dplyr)
setwd("C:/Users/antho/Desktop/2020 S2/Predictive Modelling/FinalProject")
mushroom <- read.csv("mushrooms.csv")
#veil type has only 1 unique value which makes this col redundant. Therefore drop
drop <- c("veil.type")
mushroom = mushroom[,!(names(mushroom) %in% drop)]
#create train and validation sets
set.seed(1)
train_size = floor(0.8*nrow(mushroom))
train_ind <- sample(seq_len(nrow(mushroom)), size = train_size)
nrow(train)
nrow(test)
train <- mushroom[train_ind, ]
test <- mushroom[-train_ind, ]

lda.fit = lda(train$class~., data = train)


lda.predict = predict(lda.fit, test)
table(lda.predict$class, test$class, dnn=c('Predicted class','True class'))%>%addmargins()
