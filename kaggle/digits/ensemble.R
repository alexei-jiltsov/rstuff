library(nnet)
library(e1071)


train <- read.csv("train.csv", header=TRUE)
train0 <- train
test <- read.csv("test.csv", header=TRUE)

labels <- as.factor(train[,1])
train <- train[,-1]

train.1 <- ifelse(train>10,1,0)

mynnet <- nnet(labels ~ ., data=train.1, size=15, entropy = TRUE,
            decay = 5e-04,
            MaxNWts=150000,
            maxit = 350)
mynnet.pred.train <- predict(mynnet,train.1,type="class")
mynnet.pred.test <- predict(mynnet,test,type="class")

write(mynnet.pred.test, file="mynnet.pred.test.csv", ncolumns=1) 

library(randomForest)

rf <- randomForest(train, labels, xtest=test, ntree=1000)
rf.pred <- levels(labels)[rf$test$predicted]
write(rf.pred, file="rf.pred.csv", ncolumns=1)

library(FNN)

knn.pred <- (0:9)[knn(train, test, labels, k = 10, algorithm="cover_tree")]
write(knn.pred, file="knn.pred.csv", ncolumns=1)



svm.model  <- svm(label~., data = train0)
svm.pred <- predict(svm.model, test)
svm.train.pred <- predict(svm.model, train)

write(svm.pred, file="svm.pred.csv", ncolumns=1)