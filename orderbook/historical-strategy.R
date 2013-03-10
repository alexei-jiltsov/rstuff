library(mobster)

#' Simple market making strategy.
#' Whenever the spread is wider than a target spread this strategy provides liquidity.
#' This strategy attempts to keep risk (the position size) close to zero. 
#' @param num The number of iterations to model  
#' @export
trade.historical.spy <- function( ) {

	
	orders <<- data.table(id=1:1000000, sym=rep("sym",1000000), trader=rep("t",1000000), price=rep(0,1000000), side=rep(0,1000000), qty=rep(0,1000000), tradeype=rep("tt",1000000), time=rep(0,1000000), externalId=rep(0,1000000), key="id")

	# init the book
	

	# strategies current position after filling orders
	position <<- 0
	
	# current sell quote id	
	sell.quote.id <<- NULL
	
	# current buy quote id
	buy.quote.id  <<- NULL

	# start main loop
	
	df <- read.csv('/Users/tomappleton/Downloads/20121109_GOOG.csv')
	sales <- read.csv('/Users/tomappleton/Downloads/20121109_GOOG(3).csv')
	# remove orders before opening cross
	start <- 9.5*60*60*1000
	df <- df[which(df$Time>start),]
	sales <- sales[which(sales$Time>start),]
	
	
	result <- .C("init", as.integer(1000000), as.integer(max), as.integer(min), PACKAGE='mobster')
	count <<- 1
	apply(df2, 1, trade)
	
	
}
 
# par(new=TRUE)
# plot(h[2:num]$bid0, type='s', col='green', ylim=ylim)

 # plot(format(as.POSIXlt(df$Time/1000, origin="2012-09-28"), format="%H:%M:%OS3"), get.hob(num)$ask0, type='s', col='red', ylim=ylim)
 # plot(format(as.POSIXlt(execs$Time/1000, origin="2012-09-28"), format="%H:%M:%OS3"),execs$sum, type='s', col='red', ylim=ylim)
 
# 
trade <- function(row) {

		price 		<- as.numeric(row[[6]])/10000
		size 		<- as.numeric(row[[5]])
		tradetype 	<- as.character(row[[4]])
		externalid	<- as.integer(row[[3]])
		sym 		<- as.character(row[[2]])
		time 		<- as.integer(row[[1]])
	
		count <<- 1 + count	

		if(tradetype=='D') {
			cancel(externalid, time=time)
		} else if(tradetype=='B') {
			limit(sym, "mkt", 0, price, size, "GTC", time, externalid)
		} else if(tradetype=='S') {
			limit(sym, "mkt", 1, price, size, "GTC", time, externalid)
		} else if(tradetype=='C') {
			cancel(0, qty=size, time, externalid)
		} else if(tradetype=='F') {
			cancel(externalid, time=time)
		} else if(tradetype=='E') {
			cancel(0, qty=size, time, externalid)
		} else if(tradetype=='T') {
			cancel(0, qty=size, time, externalid)
		}



}


# step over the itch ticks trades 1 by 1 for debugging 
step <- function(ticks, sales, start=0, FUN=trade, plot=TRUE, low=14300, high=14500, n=1) {
    # reset the position to the start
	if(start==1) {
	   position <<- 1
		result <- .C("init", as.integer(1000000), as.integer(0), as.integer(0), PACKAGE='mobster')
		count <<- 1
	}
	
    print(ticks[position,])
    apply(ticks[position,], 1, FUN)
    
    if(plot==TRUE) {
     # plot our version against the real version
     num = position
	 h <<- get.hob(num)
	 
 
	 sells <- sales[which(sales$T=='S'),]
	 buys <- sales[which(sales$T=='B'),]
	 
	 
	 ylim <<- c(low, high)
	 xlim <<- c(2.8e7, max(h[1:num]$time))

	 
	 plot( h[2:num]$time , h[2:num]$ask0, type='s', col='black', ylim=ylim, xlim=xlim)
	 
	 par(new=TRUE)
	 plot(sells$Time, sells$Price/100, type='s', col='red', ylim=ylim, xlim=xlim)
	 par(new=TRUE)
	 plot(buys$Time, buys$Price/100, type='s', col='green', ylim=ylim, xlim=xlim)

    }
    
    position <<- position + 1
    print(get.ob())
}


s2 <<- data.table(Time=rep(1,1),
				 T=rep('X',1),
			     Ticker=rep('SPY',1),
			     Order=rep(1,1),
			     Shares=rep(1,1),
			     Price=rep(1,1))

convert <- function(row) {

 new <- data.table(Time=rep(1,1),
				 T=rep('X',1),
			     Ticker=rep('SPY',1),
			     Order=rep(1,1),
			     Shares=rep(1,1),
			     Price=rep(1,1))
			     
	new$Ticker <- 'SPY'
	if(row[[1]] == 'A') {
		if(as.character(row[[6]])=='BID') {
				new$Price <- row[[4]]
				new$Order <- row[[3]]
				new$Time  <- row[[2]]
				new$Shares<- row[[5]]
				new$T     <- "B"	
		}
		if(as.character(row[[6]])=='ASK') {
				new$Price <- row[[4]]
				new$Order <- row[[3]]
				new$Time  <- row[[2]]
				new$Shares<- row[[5]]
				new$T     <- "S"
		}
	} else if(row[[1]] == 'C') {
	
				new$Price <- 0
				new$Order <- row[[3]]
				new$Time  <- row[[2]]
				new$Shares<- row[[5]]
				new$T     <- "C"
 	} else if(row[[1]] == 'T') {
	
				new$Price <- row[[4]]
				new$Order <- row[[3]]
				new$Time  <- row[[2]]
				new$Shares<- row[[5]]
				new$T     <- "E"
 	} else {
 		return;
 	}
 	
 	s2 <<- rbind(s2, new)
 	return
 			
}		


# hsub$bidqty2 <- ifelse(hsub$bidqty3==0,1,hsub$bidqty3)
# hsub$bidqty1 <- ifelse(hsub$bidqty1==0,1,hsub$bidqty1)

# hsub$askqty0f <- cut(log(hsub$askqty0),8)
# hsub$askqty1f <- cut(log(hsub$askqty1),8)
# hsub$askqty2f <- cut(log(hsub$askqty2),8)
# hsub$bidqty2f <- cut(log(hsub$bidqty2),8)
# hsub$bidqty1f <- cut(log(hsub$bidqty1),8)
# hsub$bidqty0f <- cut(log(hsub$bidqty0),8)
