install.packages("e1071")

library("e1071")
library(rpart)
library(rpart.plot)

######################################################################

## split data into train and test set
sslc.features = read.csv(file.choose(),header=T)
index <- 1:nrow(sslc.features)
testindex <- sample(index, trunc(length(index)/5))
testset <- sslc.features[testindex, ]
trainset <- sslc.features[-testindex, ]

## svm
svm.model <- svm(Compensation ~ ., data = testset, cost = 100, gamma = 1)

## sslc1
index <- 1:nrow(sslc1)
testindex <- sample(index, trunc(length(index)/3))
testset <- sslc1[testindex, ]
trainset <- sslc1[-testindex, ]

## svm
svm.model <- svm(Compensation ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[, -3])
svm.pred
## rpart
rpart.model <- rpart(Compensation ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[,-3], type = "class")

## compute svm confusion matrix
svm.table <- table(pred = svm.pred, true = testset[,3])
svm.table

## compute rpart confusion matrix
rpart.table <- table(pred = rpart.pred, true = testset[,3])
rpart.table
View(sslc.features)

####################################################################

## test
attach(sslc1)
x <- subset(sslc1, select = -NRC_MEDIUM)
y <- NRC_MEDIUM
model <- svm(NRC_MEDIUM ~ ., data = trainset)

print(model)
summary(model)

pred <- predict(model, x)

pred <- fitted(model)

table(pred, y)

pred <- predict(model, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

plot(cmdscale(dist(testset[,-1])),
     col = as.integer(testset[,1]),
     pch = c("o","+")[1:150 %in% model$index + 1])

plot(model, y)
library(MASS)

plot(x, y)


#######################################################################

## sslc2
index2 <- 1:nrow(sslc2)
testindex2 <- sample(index, trunc(length(index)/3))
testset2 <- sslc2[testindex, ]
trainset2 <- sslc2[-testindex, ]

## svm
svm.model2 <- svm(SCHOOL_TYPE ~ ., data = trainset2, cost = 100, gamma = 1)
svm.pred2 <- predict(svm.model2, testset2[, -1])

## rpart
rpart.model2 <- rpart(SCHOOL_TYPE ~ ., data = trainset2)
rpart.pred2 <- predict(rpart.model2, testset2[,-1], type = "class")

## compute svm confusion matrix
svm.table2 <- table(pred = svm.pred2, true = testset2[,1])

## compute rpart confusion matrix
rpart.table2 <- table(pred = rpart.pred2, true = testset2[,1])

######################################################################

## sslc3
index3 <- 1:nrow(sslc3)
testindex3 <- sample(index3, trunc(length(index3)/3))
testset3 <- sslc3[testindex3, ]
trainset3 <- sslc3[-testindex3, ]

## svm
svm.model3 <- svm(SCHOOL_TYPE ~ ., data = trainset3, cost = 100, gamma = 1)
svm.pred3 <- predict(svm.model3, testset3[, -1])

## rpart
rpart.model3 <- rpart(SCHOOL_TYPE ~ ., data = trainset3, method = "class")
rpart.pred3 <- predict(rpart.model3, testset3[,-1], type = "class")

## compute svm confusion matrix
svm.table3 <- table(pred = svm.pred3, true = testset3[,1])

## compute rpart confusion matrix
rpart.table3 <- table(pred = rpart.pred3, true = testset3[,1])

rpart.plot(rpart.model3)
svm.table3
rpart.table3


######################################################################

## sslc4
index4 <- 1:nrow(sslc4)
testindex4 <- sample(index4, trunc(length(index4)/3))
testset4 <- sslc4[testindex4, ]
trainset4 <- sslc4[-testindex4, ]

## svm
svm.model4 <- svm(URBAN_RURAL ~ ., data = trainset4, cost = 100, gamma = 1)
svm.pred4 <- predict(svm.model4, testset4[, -2])

## rpart
rpart.model4 <- rpart(URBAN_RURAL ~ ., data = trainset4, method = "class")
rpart.pred4 <- predict(rpart.model4, testset4[,-2], type = "class")

## compute svm confusion matrix
svm.table4 <- table(pred = svm.pred4, true = testset4[,2])

## compute rpart confusion matrix
rpart.table4 <- table(pred = rpart.pred4, true = testset4[,2])

rpart.plot(rpart.model4)
svm.table4
rpart.table4
sal = read.csv(file.choose(),header=T)
View(sal)
dim(sal)
class(sal$SalMid)
s<- sample(26087,1000)
s
sal_train <- sal[s,]
sal_test <- sal[-s,]
dim(sal_test)
dim(sal_train)


library(rpart)
dtm <- rpart(Compensation~.,sal_train, method="class")

dtm
plot(dtm)
text(dtm)

library(rpart.plot)
prp(dtm)
rpart.plot(dtm)
rpart.plot(dtm, type=4, extra = 101, tweak = 1.5, fallen.leaves = TRUE)
p <- predict(dtm, sal_test,type="class")

table(sal_test[,3],p)

library(rattle)


fancyRpartPlot(dtm, tweak = TRUE)