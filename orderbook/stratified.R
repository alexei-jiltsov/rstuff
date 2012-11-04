
stratified <- function() {
	file='/Users/tomappleton/Downloads/20120928_SPY.csv'
	df <- read.csv(file)

	min <- min(df$Shares)
	
	
	for(i in 1:10) {
		max <- max(df$Shares)
		df <- df[which(df$Shares!=max),]
	}
	
	trades <- df[which(df$T %in% c('B','S')),]
	executions <- df[which(df$T %in% c('E','F')),]
	trades$Shares <- ifelse(trades$T=='B',-1*trades$Shares, trades$Shares)

	plot(trades$Time, trades$Shares, pch='.')
}

# work out if ask0 or bid0 changed after 1/2/3 time periods
# then use that as the classifier vector
library(e1071)
h$mid <- ((h$ask0 + h$bid0)/2)
h$diff1 <- c(0,diff(h$mid, lag=1))
h$diff1c <- ifelse(h$diff1>0, 1, h$diff1)
h$diff1c <- ifelse(h$diff1<0, -1, h$diff1)
h$l1Ratio <- 

# time based binning - check trade volume based binning also
analyse.poisson <- function() {

	library(data.table)

	file='/Users/tomappleton/Downloads/20120928_SPY.csv'
	
	df			<- read.csv(file)
	starttime 	<- min(df$Time)
	endtime 	<- max(df$Time)
	bins 		<- seq(starttime, endtime, by=10000)
	df$bin 		<- cut(df$Time,  breaks = bins)
	trades		<- df[which(df$T %in% c('B','S')),]
	buys  		<- df[which(df$T %in% c('B')),]
	sells 		<- df[which(df$T %in% c('S')),]

	a <- as.data.frame(table(df$Shares))
	a$Var1		<- as.character(a$Var1)
	a$Var1		<- as.integer(a$Var1)
	a <- a[which(a$Var1>=100),]
	levels 		<- a[which(a$Freq>10000),]

	# count how many at each level in each bin
	x <- table(trades$bin, trades$Shares)
	x <- x[, as.character(levels$Var1)]
	y <- as.data.frame(x)
	# the data seems to have a start time and an end time
	# so we choose only data that looks valid
	start.position <- which(y[[length(y)]]>0)[[1]]
	end.position   <- last(which(y[[length(y)]]>1))
	y.reduced <- y[start.position:end.position,]
	
	# poisson doesnt actually fit well - negative binomial looks better
	library(vcd)
	gf<-goodfit(y.reduced[[1]],type='nbinomial',method='MinChisq')
	summary(gf)
	

}

# analyse the data, but bin by volume
analyse.volume.time <- function() {

	library(data.table)

	file='/Users/tomappleton/Downloads/20120928_SPY.csv'
	
	df			<- read.csv(file)
	starttime 	<- min(df$Time)
	endtime 	<- max(df$Time)
	df$vol 		<- cumsum(df$Shares)
	bins 		<- seq(0, max(df$vol), by=850000)
	df$bin 		<- cut(df$vol,  breaks = bins)
	trades		<- df[which(df$T %in% c('B','S')),]
	buys  		<- df[which(df$T %in% c('B')),]
	sells 		<- df[which(df$T %in% c('S')),]

	# now work out the levels - we are removing those that dont occur as often and may be more noisy (and difficult to understand)
	a <- as.data.frame(table(df$Shares))
	a$Var1		<- as.character(a$Var1)
	a$Var1		<- as.integer(a$Var1)
	a <- a[which(a$Var1>=100),]
	levels 		<- a[which(a$Freq>1000),]

	# count how many at each level in each bin
	x <- table(trades$bin, trades$Shares)
	x <- x[, as.character(levels$Var1)]
	y <- as.data.frame(x)
	# the data seems to have a start time and an end time
	# so we choose only data that looks valid
	start.position <- which(y[[length(y)]]>0)[[1]]
	end.position   <- last(which(y[[length(y)]]>1))
	y.reduced <- y[start.position:end.position,]
	
	# poisson doesnt actually fit well - negative binomial looks better
	library(vcd)
	gf<-goodfit(y.reduced[[1]],type='nbinomial',method='MinChisq')
	summary(gf)
	

}

groupSum <- function(start, nums, target) { 
	if(start==length(nums)) {return (target == 0)}
	if(groupSum(start+1, nums, target-nums[start])) { return (TRUE)}
  
  	if(groupSum(start+1, nums, target)) { return (TRUE) }
  
  return (FALSE);
}

	  if(start>=nums.length) return (target==0);
	  
	  int count = 0 ;
	  while(start+count < nums.length && nums[start]==nums[start+1]) {
	  
	    count++;
	    start++;
	
	  }
	  
	  if(count>0) { 
	   
	    if(groupSumClump(start+count, nums, target-count*nums[start])) {return true ;} 
	    
	    if(groupSumClump(start+count, nums, target)) return true;
	    
	    return false;
	    
	  }
	
	
	  if(groupSumClump(start+1, nums, target)) return true;
	  
	  if(groupSumClump(start+1, nums, target-nums[start])) return true; 
	  
	  return false;  




public boolean splitArray(int[] nums) {
  if(nums[0]==nums[1] && nums.length==2) return true;
  
  int[] next = Arrays.copyOfRange(nums, 1, nums.length);

  if(nums.length>2) {
    if(groupSum(nums[0], next, nums[0]) return true; 
  }
  
  if(splitArray(
  
  return false;

}

public boolean groupSum(int start, int[] nums, int target) {
  if(start==nums.length) return (target==0);

  if(groupSum(start+1, nums, target)) return true;
  
  if(groupSum(start+1, nums, target-nums[start])) return true;
  
  return false;
}
	