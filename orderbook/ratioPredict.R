# Predict moves in  an orderbook using a ratio of bid to offer for the signal
# Very simple initial model. Classify as up/down/no move
# Predict a move based on current state and look n steps into the future to see if it was accuracte
# Check the accuracy - ie the actual vs the predicted
# success should be true if the model predicts the direction correctly within <n> ticks, without a
# tick the other way before it.
# score the function for evaluation against other methods of prediction
# POTENTIAL ENHANCEMENTS : multiple classifiers (ie more than one pip) to understand strength of signal,
# bid/ask classifiers, more derived features other than simple ratio, 
# different models (svm, RFF, KNN, SOM, randomForest, NN) and ensembles

run <- function() {
   # get some data - should have Ask0, Ask1.... AskQty1, AskQty2.... Bid0, Bid1... BidQty0, BidQty1
   # depth <- data.frame()
   
   # work out the pxdiff one tick in the future. More ticks into the future may make sense (ie let a
   # machine learning algo work out if there is a big order resting in the middle of the book)
   # but there is some difficulty/noise when you start looking more than one tick - the book can go up/down
   # so how do you tell if the signal was valid ? 
   depth$mid <- (depth$Ask0+depth$Bid0)/2
   depth$pxdiff1 <- vect_lag(depth$mid1, 1) - depth$mid1
   
   # simple classifier - up/down/no move
   depth$classifier1 <- as.factor(ifelse(depth$pxdiff1>0,1,ifelse(depth$pxdiff<0,-1,depth$pxdiff1)))
   
   # construct the models/predictions
   depth$ratiopred1 <- ratioPredict(depth$AskSize0, depth$BidSize0, 20)
   
   # strict classifier confusion matrix - prediction and actual move have to be exactly the same on the same index
   # this is not ideal since the prediction may be correct when looking further into the future, but not on the current value
   level1.tab <- table(depth$classifier1, depth$ratiopred1)
   
   # work out the accuracy of the prediction n steps into the future and then score it
   # this is where we control how far we look into the future when making a prediction
   data.ratio.simple <- score.n.steps(depth$classifier1, depth$ratiopred1, 200)
   
}

# the 'model'
# threshold of 20 would mean 20:1 bid:ask means predict a move
ratioPredict <- function(ask, bid, threshold) {
   ratioBid <- bid/ask
   ratioAsk <- ask/bid
   return (ifelse(ratioBid>threshold, 1, ifelse(ratioAsk>threshold, -1, 0)))
}