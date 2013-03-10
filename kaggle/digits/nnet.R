library(nnet)

train <- read.csv("train.csv", header=TRUE)
train.1 <- train[0:21000,]
train.2 <- train[21001:42000,]
test <- read.csv("test.csv", header=TRUE)

labels <- as.factor(train[,1])
labels.1 <- labels[0:21000]
labels.2 <- labels[21001:42000]
train <- train[,-1]



# remove those with small variance
#vars <- apply(train, 2, var)
#train.s <- train[-which(vars<0.5),]

# normalise the data - choose greater than 10 to mean on, and less than to mean off 
train.101 <- ifelse(train.1>10,1,0)
train.102 <- ifelse(train.2>10,1,0)

mynnet <- nnet(labels ~ ., data=train.15, size=15, entropy = TRUE,
            decay = 5e-04,
            MaxNWts=150000,
            maxit = 350)
mynnet.pred.train <- predict(mynnet,train,type="class")
mynnet.pred.test <- predict(mynnet,test,type="class")


class.tab <- table( labels, mynnet.pred.train)
class.tab
(sum(class.tab) - sum(diag(class.tab))) / sum(class.tab)


write(p, file="nnet.csv", ncolumns=1) 

