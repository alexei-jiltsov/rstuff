library(e1071)

train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)

labels <- as.factor(train[,1])
train <- train[,-1]

bayes <- naiveBayes(labels ~ ., data=train)
p <- predict(bayes, test)

class.tab <- table( labels, p)
class.tab
(sum(class.tab) - sum(diag(class.tab))) / sum(class.tab)


write(p, file="bayes.csv", ncolumns=1) 

