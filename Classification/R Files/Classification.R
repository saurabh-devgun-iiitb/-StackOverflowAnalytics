sal = read.csv(file.choose(),header=T)
View(sal)
dim(sal)
class(sal$SalMid)
s<- sample(455,410)
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
