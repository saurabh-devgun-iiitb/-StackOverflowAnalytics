install.packages("e1071")

library("e1071")
library(rpart)
library(rpart.plot)


3#2#####################################################################

## split data into train and test set
index <- 1:nrow(sslc.features)
testindex <- sample(index, trunc(length(index)/5))
testset <- sslc.features[testindex, ]
trainset <- sslc.features[-testindex, ]

## svm
svm.model <- svm(NRC_CLASS ~ ., data = testset, cost = 100, gamma = 1)

## sslc1
index <- 1:nrow(sslc1)
testindex <- sample(index, trunc(length(index)/3))
testset <- sslc1[testindex, ]
trainset <- sslc1[-testindex, ]

## svm
svm.model <- svm(NRC_MEDIUM ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[, -1])

## rpart
rpart.model <- rpart(NRC_MEDIUM ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[,-1], type = "class")

## compute svm confusion matrix
svm.table <- table(pred = svm.pred, true = testset[,1])

## compute rpart confusion matrix
rpart.table <- table(pred = rpart.pred, true = testset[,1])