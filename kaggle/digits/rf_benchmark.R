# makes the random forest submission

library(randomForest)


train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)

labels <- as.factor(train[,1])
train <- train[,-1]

# remove those with small variance
vars <- apply(train, 2, var)
remove <- -which(vars<0.5)
train <- train[remove,]
test <- test[remove,]


rf <- randomForest(train, labels, xtest=test, ntree=1000)
predictions <- levels(labels)[rf$test$predicted]

class.tab <- table( labels, as.integer(rf$predicted))
class.tab
(sum(class.tab) - sum(diag(class.tab))) / sum(class.tab)


write(predictions, file="rf_benchmark2.csv", ncolumns=1) 
