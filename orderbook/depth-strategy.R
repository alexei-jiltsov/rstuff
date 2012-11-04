library(mobster)

# plot(get.hob(10000)$ask0, type='s')
# length(which(e$trader=="open"))
# length(which(e$trader=="clos"))

#' Trades when depth is thin in one side of the market.
#' Generates quote events according to poisson, and then analyses the book
#' to determine when to trade. Trades are taken when the depth on one side of the book
#' is larger than the other, and hence more likely to move. This strategy can be elaborated on to look at futures prices vs spot
#' or by using a bayesian updating probability of when the market will move considering multiple quote levels
#' @param num The number of iterations to model
#' @export
trade.depth <- function(num=100) {


	numobs <- num
	
	orders <<- data.table(id=1:1000000, sym=rep("sym",1000000), trader=rep("t",1000000), price=rep(0,1000000), side=rep(0,1000000), qty=rep(0,1000000), tradeype=rep("tt",1000000), key="id")
	
	# arrival rates
	askLimitTradeLambda <- 0.18
	bidLimitTradeLambda <- 0.18
	askMarketTradeLambda <- 0.08
	bidMarketTradeLambda <- 0.08
	ask0QuoteLambda <- 0.31
	bid0QuoteLambda <- 0.31
	ask1QuoteLambda <- 0.22
	bid1QuoteLambda <- 0.22

	# model arrivals
	askLimitTradeEvents <- abs(round(rpois(numobs, askLimitTradeLambda) * rnorm(numobs, 4)))
	bidLimitTradeEvents <- abs(round(rpois(numobs, bidLimitTradeLambda) * rnorm(numobs, 4)))
	askMarketTradeEvents <- abs(round(rpois(numobs, askMarketTradeLambda) * rnorm(numobs, 4)))
	bidMarketTradeEvents <- abs(round(rpois(numobs, bidMarketTradeLambda) * rnorm(numobs, 4)))
	ask0QuoteEvents <- abs(round(rpois(numobs, ask0QuoteLambda) * rnorm(numobs, 2)))
	bid0QuoteEvents <- abs(round(rpois(numobs, bid0QuoteLambda) * rnorm(numobs, 2))) 
	ask1QuoteEvents <- abs(rpois(numobs, ask1QuoteLambda))
	bid1QuoteEvents <- abs(rpois(numobs, bid1QuoteLambda))
	
	# init the book
	result <- .C("init", as.integer(num), as.integer(10000), as.integer(9999), PACKAGE='mobster')


	stopPrice <- -1
	lastOrderId <- -1
	lastStopId <- -1
	fillqty <- -1
	side <- -1

	for (i in 1:numobs) {
	
		ob <- get.ob()
		
		bestAsk <- ob[10,]
		bestBid <- ob[11,]
	
		 
		spread <- ob[10,]$price - ob[11,]$price  

		if(spread >0.011) {
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
		# when to trade ? when depth is much higher on one side  
		# how to choose when to close ? take profit of 1 pip. Can optimise with a stop loss for trades where the market moves
		# too far away
		
		ob <- get.ob()
		
		bidqty <- ob[11,]$qty
		askqty <- ob[10,]$qty
		
		
		if(askqty>0 & bidqty>0) {
			if( (bidqty/askqty) < 0.25) {
				# bid (people offering to buy) is thin - p more likely to move down - so we sell (take their buyprice) and place a limit buy with take profit 1 pip below
				lastOrderId <- limit("sym","open", 1, ob[11,]$price, 1, "GTC")
				side <- 0
				if(lastOrderId>0) {				
					order <- orders[lastOrderId,]
					lastOrderId <- limit("sym", "close", 0, order$price-0.01, 1, "GTC")
					lastStopId <- limit("sym", "stop", 0, order$price+0.01, 1, "GTC")
				}			
			} else if( (askqty/bidqty) < 0.25) {
				# ask (people offering to sell) is thin - p more likely to move up - so we buy (take their sellprice) and place a limit sell with take profit 1 pip above
				lastOrderId <- limit("sym","open", 0, ob[10,]$price, 1, "GTC")
				side <- 1
				if(lastOrderId>0) {
					order <- orders[lastOrderId,]			
					lastOrderId <-limit("sym", "close", 1, order$price+0.01, 1, "GTC")
					lastStopId <-limit("sym", "stop", 1, order$price-0.01, 1, "GTC")
				}	
			}
			
		}
		
		if(lastOrderId > -1) {
			fillqty <- get.filled.qty(lastOrderId)
			cancel(lastStopId)
		}
		
		if(lastStopId > -1) {
			fillqty <- get.filled.qty(lastOrderId)
			cancel(lastOrderId)
		} 
		
		
		#print(paste('lastId', lastOrderId, 'fillqty', fillqty, 'stopPrice', stopPrice, 'askprice', ob[10,]$price, 'bidprice', ob[11,]$price, 'side', side))
		
		ob <- get.ob()

	
		# market trade arrival
		if(askMarketTradeEvents[i]>0) {
			limit("sym", "poisson", 1, ob[13,]$price, askMarketTradeEvents[i], "GTC")
			#market("sym", "poi", 1, 100.00, askMarketTradeEvents[i])
		}
		if(bidMarketTradeEvents[i]>0) {
			limit("sym", "poisson", 0, ob[7,]$price, bidMarketTradeEvents[i], "GTC")
			#market("sym", "poi", 0, 100.00, bidMarketTradeEvents[i])
		}
	}
}



