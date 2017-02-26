Iris = read.csv(file.choose(),header=T)
View(Iris)
Iris.features = Iris
Iris.features$class <- NULL
View(Iris.features)
results = kmeans(Iris.features, 3)
results
results$size
results$cluster
table(Iris$class, results$cluster)
plot(Iris[c("petal.length","petal.width")], col = results$cluster)
plot(Iris[c("petal.length","petal.width")], col = Iris$class)


#Classification
#install.packages("rpart.plot")
iris = read.csv(file.choose(),header=T)
View(Iris)
dim(Iris)
s<- sample(150,100)
s
iris_train <- iris[s,]
iris_test <- iris[-s,]
dim(iris_test)
dim(iris_train)

library(rpart)
dtm <- rpart(Species~.,iris_train, method="class")
dtm <- rpart(class~sepal.length+sepal.width+petal.length+petal.width,iris_train, method="class")
dtm
plot(dtm)
text(dtm)
library(rpart.plot)
rpart.plot(dtm)
rpart.plot(dtm, type=4, extra = 101)
p <- predict(dtm, iris_test,type="class")
table(iris_test[,5],p)

install.packages("RGtk2")
