
function(x, ...) {
  H <- hist(x, plot = FALSE)
  H$density <- with(H, 100 * density* diff(breaks)[1])
  labs <- paste(round(H$density), "%", sep="")
  plot(H, freq = FALSE, labels = labs, ylim=c(0, 1.08*max(H$density)),...)
}

tseq <- seq(now, length.out = 100000000, by = 1)
ts <- sort(sample(tseq,50000))

## plot the tick infrequency. This is  measure of the average duration
## between ticks. This is plotted in a heatmap of day of the week vs time
## to show patterns. The size of the duration can be specified so you can look for
## times of day that are likely to have long pauses between ticks
##
## ts - the timeseries of data
## incr - the size of the databins in minutes
## dur - the length between ticks (for filtering)
tickInfrequency <- function(ts, incr=10, dur=0) {
  
  if(((60/incr)%%1)>0) {
    stop("time increment must be exact divisor of 60 (no remainder)")
  }
  
  ## add NA for last column
  diffs <- c(as.numeric(diff(ts)),NA)
  incr <- incr - incr%%1
  ## bin by the increment
  if(incr<1) {
    incr = 1
  }
  ## work out the expected intervals
  if(incr>=60) {
    minbin <- rep(60, length(ts))  
    incr <- 60    
  } else {
    minbin <- cut(as.numeric(strftime(ts, "%M")), breaks=60/incr)
  }
  incrs <- seq(from=incr, to=60, by=incr)
  alltimes <- c()
  for(i in 0:23) {
    alltimes <- c(alltimes, paste(i,incrs,sep=':'))
  }
  tt <- data.table(time=alltimes)

  hour <- as.numeric(strftime(ts, "%H"))
  diffs <- diffs[which(diffs>dur)]
  
  df <- data.frame(time=ts, diff=diffs, day=weekdays(ts), mins=minbin, hours=hour)
  ## remove NA
  df <- df[1:length(df$time)-1,]
  dt <- data.table(df)
  ## calculate the mean of the time diffs for each bin
  dt <- dt[, mean(diff), by=list(day,hours,mins)]
  dt$day <-as.character(dt$day)
  setnames(dt, "V1", "mean")
  if(incr==60) {
    dt$mins <- as.numeric(dt$mins)
  } else {
    dt$mins <- as.numeric(dt$mins)*incr
  } 
  
  dt$time <- paste(dt$hours,dt$mins,sep=':')
  setkey(dt, time)
  setkey(tt, time)
  ## sometimes there is not enough data for the bin size,
  ## so we assume there was no tick for the whole of that increment => the size
  ## should be incrementSize * 60 for 60 seconds
  mon <- dt[day=="Monday"][tt]
  if(any(is.na(mon$mean))) {
    mon[which(is.na(dt[day=="Monday"][tt]$mean))]$mean <- incr*60
  }
  tue <- dt[day=="Tuesday"][tt]
  if(any(is.na(tue$mean))) {
    tue[which(is.na(dt[day=="Tuesday"][tt]$mean))]$mean <- incr*60
  }
  wed <- dt[day=="Wednesday"][tt]
  if(any(is.na(wed$mean))) {
    wed[which(is.na(dt[day=="Wednesday"][tt]$mean))]$mean <- incr*60
  }
  thu <- dt[day=="Thursday"][tt]
  if(any(is.na(thu$mean))) {
    thu[which(is.na(dt[day=="Thursday"][tt]$mean))]$mean <- incr*60
  }
  fri <- dt[day=="Friday"][tt]
  if(any(is.na(fri$mean))) {
    fri[which(is.na(dt[day=="Friday"][tt]$mean))]$mean <- incr*60
  }
  sat <- dt[day=="Saturday"][tt]
  if(any(is.na(sat$mean))) {
    sat[which(is.na(dt[day=="Saturday"][tt]$mean))]$mean <- incr*60
  }
  sun <- dt[day=="Sunday"][tt]
  if(any(is.na(sun$mean))) {
    sun[which(is.na(dt[day=="Sunday"][tt]$mean))]$mean <- incr*60
  }
  
  rows <- 24*60/incr
  
  mt <- matrix(nrow=rows, ncol=7, data=c(sun$mean, mon$mean, tue$mean, wed$mean, thu$mean, fri$mean, sat$mean),
               dimnames=list(alltimes,c("sun","mon","tue","wed","thu","fri","sat")))
  
  heatmap(mt, Rowv = NA, Colv = NA, scale="column", main = "tick infrequency by time of day")
}


## plot the tick frequency. This is  measure of the average count of
## tick updates in a time period. This is plotted in a heatmap of day of the week vs time
## to show patterns.
##
## ts - the timeseries of data
## incr - the size of the databins in minutes
## count - the min number of ticks (for filtering)
tickFrequency <- function(ts, incr=10, count=0) {
  
  if(((60/incr)%%1)>0) {
    stop("time increment must be exact divisor of 60 (no remainder)")
  }
  
  incr <- incr - incr%%1
  ## bin by the increment
  if(incr<1) {
    incr = 1
  }
  ## work out the expected intervals
  if(incr>=60) {
    minbin <- rep(60, length(ts))  
    incr <- 60    
  } else {
    minbin <- cut(as.numeric(strftime(ts, "%M")), breaks=60/incr)
  }
  incrs <- seq(from=incr, to=60, by=incr)
  alltimes <- c()
  for(i in 0:23) {
    alltimes <- c(alltimes, paste(i,incrs,sep=':'))
  }
  tt <- data.table(time=alltimes)
  
  hour <- as.numeric(strftime(ts, "%H"))
  dt <- data.table(time=ts, day=weekdays(ts), mins=minbin, hours=hour)
  setkey(dt,day,hours,mins)
  ## calculate the count for each time bucket
  dt$day <-as.character(dt$day)
  
  ## calculate counts by group
  t <- data.table(data.frame(xtabs(~day+hours+mins, dt)))
  t$mins <- as.numeric(t$mins)*incr
  t$time <- paste(t$hours,t$mins,sep=':')
  
  setkey(t, time)
  setkey(tt, time)
  
  ## sometimes there is not enough data for the bin size,
  ## so we assume there was no tick for the whole of that increment => the size
  ## should be incrementSize * 60 for 60 seconds
  mon <- t[day=="Monday"][tt]
  tue <- t[day=="Tuesday"][tt]
  wed <- t[day=="Wednesday"][tt]
  thu <- t[day=="Thursday"][tt]
  fri <- t[day=="Friday"][tt]
  sat <- t[day=="Saturday"][tt]
  sun <- t[day=="Sunday"][tt]
  
  rows <- 24*60/incr
  
  mt <- matrix(nrow=rows, ncol=7, data=c(sun$Freq, mon$Freq, tue$Freq, wed$Freq, thu$Freq, fri$Freq, sat$Freq),
               dimnames=list(alltimes,c("sun","mon","tue","wed","thu","fri","sat")))
  
  heatmap(mt, Rowv = NA, Colv = NA, scale="column", main = "tick infrequency by time of day")
}
