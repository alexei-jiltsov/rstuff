library(e1071)

train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)

labels <- as.factor(train[,1])
train <- train[,-1]

mysvm <- svm(labels ~ ., data=train)
svm.predict.train <- predict(mysvm, train)
svm.predict.test <- predict(mysvm, test)

class.tab <- table( labels, svm.predict.train)
class.tab
(sum(class.tab) - sum(diag(class.tab))) / sum(class.tab)


write(p, file="bayes.csv", ncolumns=1) 