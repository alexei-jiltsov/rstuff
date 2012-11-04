# plot(get.hob(10000)$ask0, type='s')
# length(which(e$trader=="open"))
# length(which(e$trader=="clos"))

#' simple mavg trading for comparison (ie to show how terribly it performs!)
#' generate quote events according to poisson, and then use moving avg to enter/exit positions
#' @param num The number of iterations to model
#' @export
trade.mavg <- function(num=100) {

	ppnl <- 0
	lprice <- 0

	numobs <- num
	
	orders <<- data.table(id=1:1000000, sym=rep("sym",1000000), trader=rep("t",1000000), price=rep(0,1000000), side=rep(0,1000000), qty=rep(0,1000000), tradeype=rep("tt",1000000), key="id")
	
	# arrival rates
	askLimitTradeLambda <- 0.18
	bidLimitTradeLambda <- 0.18
	askMarketTradeLambda <- 0.08
	bidMarketTradeLambda <- 0.08
	ask0QuoteLambda <- 0.28
	bid0QuoteLambda <- 0.28
	ask0CancelLambda <- 0.15
	bid0CancelLambda <- 0.15
	ask1QuoteLambda <- 0.19
	bid1QuoteLambda <- 0.19
	ask1CancelLambda <- 0.25
	bid1CancelLambda <- 0.25

	# model arrivals
	askLimitTradeEvents <- abs(round(rpois(numobs, askLimitTradeLambda) * rnorm(numobs, 6)))
	bidLimitTradeEvents <- abs(round(rpois(numobs, bidLimitTradeLambda) * rnorm(numobs, 6)))
	askMarketTradeEvents <- abs(round(rpois(numobs, askMarketTradeLambda) * rnorm(numobs, 6)))
	bidMarketTradeEvents <- abs(round(rpois(numobs, bidMarketTradeLambda) * rnorm(numobs, 6)))
	ask0QuoteEvents <- abs(round(rpois(numobs, ask0QuoteLambda) * rnorm(numobs, 2)))
	bid0QuoteEvents <- abs(round(rpois(numobs, bid0QuoteLambda) * rnorm(numobs, 2))) 
	ask0CancelEvents <- abs(rpois(numobs, ask0CancelLambda))
	bid0CancelEvents <- abs(rpois(numobs, bid0CancelLambda))
	ask1QuoteEvents <- abs(rpois(numobs, ask1QuoteLambda))
	bid1QuoteEvents <- abs(rpois(numobs, bid1QuoteLambda))
	ask1CancelEvents <- abs(rpois(numobs, ask1CancelLambda))
	bid1CancelEvents <- abs(rpois(numobs, bid1CancelLambda))
	
	# init the book
	result <- .C("init", as.integer(num), as.integer(10000), as.integer(9999), PACKAGE='mobster')

	# the moving average
	period <- 40
	coeff <- exp(-1.0 * (1 / period));
	mavg <- 100
	
	# track the position
	position <- 0
	# the size of each trade
	size <- 1
	
	for (i in 1:numobs) {
	
		ob <- get.ob()
		
		bestAsk <- ob[10,]
		bestBid <- ob[11,]
	
		 
		spread <- ob[10,]$price - ob[11,]$price  
		if(spread >0.01) {
			# randomly choose to fill bid or ask (assumes the market will supply liquidity at the best price)
			if(rnorm(1,0) > 0) {
				limit("sym", "poisson", 1, bestBid$price+0.01, ask0QuoteEvents[i], "GTC")
				limit("sym", "poisson", 1, bestBid$price+0.02, ask1QuoteEvents[i], "GTC")
			} else {
				limit("sym", "poisson", 0, bestAsk$price-0.01, bid0QuoteEvents[i], "GTC")
				limit("sym", "poisson", 0, bestAsk$price-0.02, bid1QuoteEvents[i], "GTC")
			}
		} else {
			# normal quote arrival
			limit("sym", "poisson", 1, ob[10,]$price, ask0QuoteEvents[i], "GTC")
			limit("sym", "poisson", 0, ob[11,]$price, bid0QuoteEvents[i], "GTC")
			limit("sym", "poisson", 1, ob[9,]$price, ask1QuoteEvents[i], "GTC")
			limit("sym", "poisson", 0, ob[12,]$price, bid1QuoteEvents[i], "GTC")
		}		
		
		# external limit trade arrival
		limit("sym", "poisson", 1, ob[11,]$price, askLimitTradeEvents[i], "GTC")
		limit("sym", "poisson", 0, ob[10,]$price, bidLimitTradeEvents[i], "GTC")
			

		# our trade
		# when to trade ? when price is above or below mavg  
		# how to choose when to close ? when price crosses back across mavg
		
		ob <- get.ob()
		
		mid <- (ob[10,]$price + ob[11,]$price)/2
		mavg <- (1.0 - coeff) * mid + coeff * mavg;
		
		# you might think that changing the > to < would make the strategy profitable 
		if(mid > mavg) {
			# buy
			if(position<=0) {
				# close open position
				id <- market("sym", "cs", 0, ob[10,]$price, -position)
				if(id > 0 && orders[id]$qty) {
					position <- position + orders[id]$qty
				}
			}
			if(position==0) {
				id <- market("sym", "ob", 0, ob[10,]$price, size)
				if(id > 0 && orders[id]$qty) {
					position <- position + orders[id]$qty
				}
			}
		} else {
			# sell
			if(position>=0) {
				# close open position
				id <- market("sym", "cb", 1, ob[10,]$price, position)
				if(id > 0 && orders[id]$qty>0) {
					position <- position - orders[id]$qty
				}
			}
			if(position==0) {
				id <- market("sym", "os", 1, ob[10,]$price, size)
				if(id > 0 && orders[id]$qty>0) {
					position <- position - orders[id]$qty
				}
			}			
		}		

	
		# market trade arrival
		if(askMarketTradeEvents[i]>0) {
			market("sym", "poi", 1, 100.00, askMarketTradeEvents[i])
		}
		if(bidMarketTradeEvents[i]>0) {
			market("sym", "poi", 0, 100.00, bidMarketTradeEvents[i])
		}
	}
	
	e <- get.execs(numobs*2)
	setkey(e, 'id', 'oid', 'mid', 'trader')
	e <- e[trader%in%c('ob','cb','os','cs')]
	
	# work out the profit
	print(sum(apply(e, 1, trade.mavg.sumpnl)))
}

#' sum the profit and loss
#' perform some analysis on the moving avg strategy. It uses the trader id
#' for the different scenarios traded (cs='close sell',cb='close buy',os='open sell',ob='open buy')
#' and calculates where the trade was entered and exited.
#' @param x the executions we made
#' @export
trade.mavg.sumpnl <- function(x) {
	if(x[[5]]=='cs') {
		pnl <- lprice - as.double(x[[7]]) 
	}
	if(x[[5]]=='ob') {
		lprice <<- as.double(x[[7]])
	}   
	if(x[[5]]=='cb') {
		pnl <- as.double(x[[7]]) - lprice
	}					
	if(x[[5]]=='os') {
		pnl <- as.double(x[[7]]) - lprice
	}
	return(pnl)					
}



