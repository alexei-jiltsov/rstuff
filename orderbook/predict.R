############################################################################
# Functions for evaluating predictive signals from orderbook state
############################################################################

# Main Classifier Function.
# A less strict classifier function than typical tabulate/classify. It is true if the predicted classifer is the same as the observed value within a few ticks (and no opposite tick before)
# ie it scans ahead to check if the prediction was correct.
# This function is called to work out how accurate a set of predictons was.
# The score function then uses this accuracy rating to score the prediction method.
# The accuracy for a signal prediction can be an enumerated value, such as 'MissedMoveDown', 'MissedMoveUp'. The meaning
# of which should be fairly self explanatory.
# x is a vector of observed up/down/no moves
# x2 is a vector of predicted up/down/no moves
classify.accuracy <- function(x, x2, i=0) {
   # check for Miss or NoAction
	if(x2==0) {
		if(length(which(x==-1)) >0) {
			return('MissedMoveDown')
		} else if(length(which(x==1))>0) {
			return('MissedMoveUp')
		} else {
			return('CorrectBothNoMove')
		}
	} else {
		if(x2==1) {
			if(length(which(x==1))>0) {
				if(length(which(x==-1))==0) {
					return('Hit-UpMove')
				} else if(length(which(x==-1)) >0 && which(x==1)[[1]]<which(x==-1)[[1]]) {
					return('Hit-UpMove')
				} else {
					return('WrongPredictedUpGotDown')
				}
			} else if(length(which(x==-1)) >0) {
				return('WrongPredictedUpGotDown')
			} else {
				return('WrongPredictedUpGotZero')
			}
		}
		if(x2==-1) {
			if(length(which(x==-1))>0) {
				if(length(which(x==1))==0) {
					return('Hit-DownMove')
				} else {
					return('WrongPredictedUpGotDown')
				}
			} else if(length(which(x==1))>0) {
				return('WrongPredctedDownGotUp')
			} else {
				return('WrongPredictedDownGotZero')
			}
		}
		if(x2==0) {
			if(length(which(x==-1)) >0 && length(which(x==1))>0) {
				return('WrongPredictedNoMoveGotBoth') 
			} else if(length(which(x==1)) >0) {
				return('MissedMoveUp') 
			} else if(length(which(x==-1))>0) {
				return('MissedMoveDown') 
			} else {
				return('CorrectBothNoMove')
			}
		}
		return('unknown')
	}
	return('unknown2')
}

# Score Function
# score the accuracy values so we can compare performance between different prediction methods. Score factors can be adjusted.
# We create a 'score' for the prediction vs the 'score' for doing nothing and missing the market moves. 
# If predicted is higher than doing nothing then we think the prediction has some value.
# The score function only considers the signal once and scans ahead to see if it is ever correct in the future.
# Basically it doesnt double count subsequent values in the array that are the same signal.
# That is as soon as we see a signal we check n places into the future to see if it was correct.
score <- function(accuracy, moves, predictions, result=character(1)) {
   total <- length(accuracy)
   moves <- as.integer(as.character(moves))

   # assign a sequence to the signals so that we have a way to only evaluate them once
   # if the signal has been checked already then dont rescore it
   # new sequence is allocated if the signal changes	
   signal <- ifelse(x <- as.integer(as.character(predictions)) %in% c(-1,1), cumsum(c(head(x,1), tail(x,-1) -head(x,-1)==1)),NA)

   # empty the signals map
   signals <<- list()

   # work out the cost of missing all the moves - this is used to compare to what our prediction would have donw
   ALL_MOVES <- sum(abs(as.integer(as.character(moves))), na.rm=TRUE)
   doNothing <- (ALL_MOVES/total)*100*-1

   # loop over each record and filter out those we have already seen	
   pred_accuracy <- vapply(1:length(accuracy), function(i) vscore(i,accuracy, signal[[i]], moves[i]]), result)

   t <- table(pred_accuracy)

   print(t)

   # tabulate the accuracy and score
   # penalise incorrect predictions & missed moves   
   CBNM <- 0 # correct both prediction and actual was no move 	
   HDM <- t["Hit-DownMove"][[1]]*3
   HUM <- t["Hit-UpMove"][[1]]*3
   MMD <- ifelse(!is.na(t["MissedMoveDown"][[1]]*), t["MissedMoveDown"][[1]]*-1,0)
   MMU <- ifelse(!is.na(t["MissedMoveUp"][[1]]*), t["MissedMoveUp"][[1]]*-1,0)
   WDU <- ifelse(!is.na(t["WrongPredictedDownGotUp"][[1]]*), t["WrongPredictedDownGotUp"][[1]]*-3,0)
   WDZ <- ifelse(!is.na(t["WrongPredictedDownGotZero"][[1]]*), t["WrongPredictedDownGotZero"][[1]]*-1,0)
   WUD <- ifelse(!is.na(t["WrongPredictedUpGotDown"][[1]]*), t["WrongPredictedUpGotDown"][[1]]*-3,0)
   WUZ <- ifelse(!is.na(t["WrongPredictedUpGotZero"][[1]]*), t["WrongPredictedUpGotZero"][[1]]*-1,0)

   predicted <- ((CBNM+HDM+HUM+MMD+MMU+WDU+WDZ+WUD+WUZ)/total)*100

   d <- data.frame(predicted.score=predicted, donothing.score=doNothing)
   print(d)
   return(d)
}

# rolling window apply function over 2 vectors - this front pads.
# n is the window size
# act has trhe window applied, pred is used for the specific index
slideapply <- function(act, pred, n, FUN, result=character(1)) {
   stopifnot(length(act) >= n)
   stopifnot(length(pred) >= n)
   FUN <- match.fun(FUN)
   nm1 <- n-1L
   y <- vapply(n:length(act), function(i) FUN(act[i:(i+nm1)], pred[i], i, pred, act), result)
   c(rep(NA,nm1),y)
}

# fn for scoring in vapply - faster than for loop
vscore <- function(i, accuracy, signal, move) {
   if(is.na(accuracy[[i]])) {
      return("0")
   }
   if(is.na(signal)) {
      # if there is no signal, but there was a move then we missed it....
      if(is.na(move) || move==0) {
         return("0")
      } else {
         return(accuracy[[i]])
      }
   }
   # check to see if this signal has already been tested
	if(is.null(signals[signal][[1]])) {
		signals[signal] <<- TRUE
		return(accuracy[[i]])
	}
	return("0")
}

# so we can lead or lag a series. ie compare current value with value x places in the past/future
vect_lag <- function(v, n=1, forward=TRUE) {
	if(forward) 
	   c(v[(n+1):length(v)], rep(NA,n))
	else 
	   c(rep(NA, n), v[1:length(v) -n)])
}

# score the function n steps into the future
# convenience function for running larger scripts to get lots of data
score.n.steps <- function(moves, predictions, n) {
   d <- data.frame()
   for(i in 1:n) {
      accuracy <- slideapply(as.integer(as.character(moves)), as.integer(as.character(predictions)), i, classify.accuracy)
      s <- score(accuracy, moves, predictions)
      d <- rbind(d,s)
   }
   return(d)
} 


 