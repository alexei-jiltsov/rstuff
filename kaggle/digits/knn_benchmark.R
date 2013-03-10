# makes the KNN submission

library(FNN)

train <- read.csv("../data/train.csv", header=TRUE)
test <- read.csv("../data/test.csv", header=TRUE)

labels <- train[,1]
train <- train[,-1]

knn.pred <- (0:9)[knn(train, test, labels, k = 10, algorithm="cover_tree")]

class.tab <- table( labels, as.integer(knn.pred))

write(results, file="knn_benchmark.csv", ncolumns=1) 
