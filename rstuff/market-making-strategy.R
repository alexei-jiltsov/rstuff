# num = 1000
# ylim <- c(min(get.hob(num)$bid0), max(get.hob(num)$ask0))
# plot(get.hob(num)$ask0, type='s', col='red', ylim=ylim)
# par(new=TRUE)
# plot(get.hob(num)$bid0, type='s', col='green', ylim=ylim)
# length(which(e$trader=="open"))
# length(which(e$trader=="clos"))

#' Simple market making strategy.
#' Whenever the spread is wider than a target spread this strategy provides liquidity.
#' This strategy attempts to keep risk (the position size) close to zero. 
#' @param num The number of iterations to model  
#' @export
trade.mm <- function(num=100) {
	numobs <- num
	
	orders <<- data.table(id=1:1000000, sym=rep("sym",1000000), trader=rep("t",1000000), price=rep(0,1000000), side=rep(0,1000000), qty=rep(0,1000000), tradeype=rep("tt",1000000), key="id")

	# create an orderbook using poisson distribution to model the trade arrival.
	# initialise poisson variables:
	
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

	# strategies current position after filling orders
	position <- 0
	
	# current sell quote id	
	sell.quote.id <- NULL
	
	# current buy quote id
	buy.quote.id  <- NULL

	# start main loop
	for (i in 1:numobs) {
	
		ob <- get.ob()
		
		bestAsk <- ob[10,]
		bestBid <- ob[11,]	
		 
		spread <- ob[10,]$price - ob[11,]$price
		  		 
		# this is our trade opportunity. If the spread is wide we can provide liquidity  		 
		if(spread >0.011) {
			# choose whether to offer at the bid or the ask based on current position
			if(position > 0) {
				# we are positive - offer to sell
				if(is.null(sell.quote.id)) {
					id <- limit("sym", "sell", 1, bestAsk$price-0.01, 1, "GTC")
					sell.quote.id <- id
				}
			} else if(position <0) {
				# we are negative - offer to buy
				if(is.null(buy.quote.id)) {
					id <- limit("sym", "buy", 0, bestBid$price+0.01, 1, "GTC")
					buy.quote.id <- id
				}
			} else if(position ==0 && spread >0.021) {
				# we are flat - offer both
				if(is.null(sell.quote.id)) {
					id <- limit("sym", "bos", 1, bestAsk$price-0.01, 1, "GTC")
					sell.quote.id <- id
				}
				if(is.null(buy.quote.id)) {
					id <- limit("sym", "bob", 0, bestBid$price+0.01, 1, "GTC")
					buy.quote.id <- id
				}
			}
		} else {
			# normal quote arrival 
			limit("sym", "poisson", 1, ob[10,]$price, ask0QuoteEvents[i], "GTC")
			limit("sym", "poisson", 0, ob[11,]$price, bid0QuoteEvents[i], "GTC")
			limit("sym", "poisson", 1, ob[9,]$price, ask1QuoteEvents[i], "GTC")
			limit("sym", "poisson", 0, ob[12,]$price, bid1QuoteEvents[i], "GTC")
		}		
		
		# check for fills and adjust the position
		if(!is.null(sell.quote.id)) {
			fillqty <- get.filled.qty(sell.quote.id)
			if(fillqty >0) {
				order <- orders[sell.quote.id]
				if(order$side==0) {
					position <- position + 1
				} else {
					position <- position - 1
				}
				sell.quote.id <- NULL
			}
		}
		if(!is.null(buy.quote.id)) {	
			fillqty <- get.filled.qty(buy.quote.id)
			if(fillqty >0) {
				order <- orders[buy.quote.id]
				if(order$side==0) {
					position <- position + 1
				} else {
					position <- position - 1
				}
				buy.quote.id <- NULL
			} 			
		}
		
		
		# external limit trade arrival
		limit("sym", "poisson", 1, ob[11,]$price, askLimitTradeEvents[i], "GTC")
		limit("sym", "poisson", 0, ob[10,]$price, bidLimitTradeEvents[i], "GTC")

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

#' analyse the market making strategy trades.
#' this function gets all the executions and extracts the deals done by
#' the market making function. The pnl analysis calculates the spread pnl (how much is earned by providing liquidity)
#' and the costs of inventory holding.  
#' @param numobs The number of observations to analyse
#' @export
trade.mm.analyse <- function(numobs) {
	e <- get.execs(numobs * 10)
	e <- e[trader != '']
	
	e$side_multiplier <- ifelse(e$side==1,-1,1)
	
	# profit due to spread
	e$spread_pnl <- e$fillqty * (ifelse(e$side==1, e$price-e$midp, e$midp-e$price))
	
	# set the mid2mid pnl for later
	e$mid2mid_pnl <- e$fillqty * 0
	
	# only look at our trades
	e$spread_pnl <- ifelse(e$trader %in% c('bos','bob','buy','sell'), e$spread_pnl, 0)
	e$side_multiplier <- ifelse(e$trader %in% c('bos','bob','buy','sell'), e$side_multiplier, 0)

	e$pos <- cumsum(e$side_multiplier)
	
	# calculate the costs of holding (ie mid2mid movement)
	# mm represents 'our' trades
	mm <- e[trader %in% c('bos','bob','buy','sell')]
	
	# calculate mid2mid pnl (holding costs)	
	matched.trades <<- list()
	open.trades <<- list()	                              
	for(i in 1:length(mm)) {
		trade <- mm[i]
		trade$riskdecreasing <- 0
		trade$riskincreasing <- 0
		trade$fill.remaining <- trade$fillqty
		
		# work out if risk reducing. If risk reducing then match it
		# sum the open position. Can be +ve or -ve based on the side. All open.trades should have the same side
		
		
		if(length(open.trades)==0) {
			print('NO TRADES OPEN')
			trade$riskincreasing <- trade$fill.remaining
			open.trades[[length(open.trades)+1]] <<- trade
			next
		}
		
		print(paste('open',open.trades))
		total.position <- sumtrades(open.trades, ifelse(open.trades[[1]]$side==0,1,-1))
		pos.sign <- sign(total.position)
		trade.sign <- sign(ifelse(trade$side==0,1,-1))
		print(paste('tradesign',trade.sign, 'pos.sign', pos.sign))
		riskreducing <- FALSE
		if(pos.sign != trade.sign && abs(trade.sign)>0 && abs(pos.sign)>0) {
			riskreducing <- TRUE     
		}
		
		
		# TODO! only return the matched trades - figure out why not all trades end up matched
		# TODO! force final trade to match at the current midp
		# TODO! make midp decimal
		# TODO! add stop loss to strategies that have large mid2mid pnl
		# TODO! add cummulative pnl
		# TODO! add total realized/unrealized pnl at each point ?
		
		
		print(trade)
		print(' ')
		print(' ')
		
		if(riskreducing) {
			print('riskreducing')
			# match
			j <- 1
			while(trade$fill.remaining >0 && length(open.trades) > 0) {
				print('loop')
				
				matchable <- open.trades[[j]]
				matched <- min(matchable$fill.remaining,trade$fillqty) 
				matchable$fill.remaining <- matchable$fill.remaining - matched
				matchable$mid2mid_pnl <- matchable$mid2mid_pnl + (trade$midp - matchable$midp) * ifelse(matchable$side==0,1,-1)  
				if(matchable$fill.remaining==0) {
					# closed - record the pnl
					print(paste('fully matched'))
					print((matchable))
					open.trades[[1]] <<- NULL
					matched.trades[[length(matched.trades)+1]] <<- matchable
					matched.trades[[length(matched.trades)]]$close.price <<- trade$midp
				}
				trade$fill.remaining <- trade$fill.remaining - matched
				trade$riskdecreasing <- trade$riskdecreasing + matched  
				j <- j + 1
			}
		}
	
		# add remaining to the list of open trades	
		if(trade$fill.remaining > 0) {
			trade$riskincreasing <- trade$fill.remaining
			open.trades[[length(open.trades)+1]] <<- trade
		}

	}			                    
	
	# This plots the cummulative pnl for the mm strategy. Other trades are not included in the calcs
	pnl <- sum(e$spread_pnl)  
	plot(cumsum(e$spread_pnl), main=paste('total pnl=',pnl), type='s', xlab='num trades', ylab='cummulative spread pnl')
	dev.new()
	plot(cumsum(e$mid2mid_pnl), main=paste('total pnl=',pnl), type='s', xlab='num trades', ylab='cummulative m2m pnl')
	return(e)
}

sumtrades <- function(trades, side) {
	sum <-0

	for(i in 1:length(trades)) {
		sum <- sum + trades[[i]]$fill.remaining
	}
	return(sum*side)
}



