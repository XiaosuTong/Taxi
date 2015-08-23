library(lattice)
source("~/Rhipe/rhinitial.R")

par <- list()
par$pace <- FALSE
par$weighted <- FALSE
if(par$pace & par$weighted){
	file <- "hour.wpacecounts"
}else if(par$pace & !par$weighted){
	file <- "hour.pacecounts"
}else{
	file <- "hourcounts"
}
data.dir <- "/ln/tongx/taxi"

rst <- rhread(file.path(data.dir, file))
result <- do.call("rbind", lapply(rst, "[[", 1))
result <- data.frame(result, stringsAsFactors=FALSE)
names(result) <- c("woy", "dow", "hour", "month", "date")
if(!par$pace){
	result$count <- unlist(lapply(rst, "[[", 2))
}else{
	result <- cbind(result, do.call(rbind, lapply(rst, "[[", 2)))
}
result$woy <- as.numeric(result$woy)
result$hour <- as.numeric(result$hour)
if(!par$pace){
	result <- rbind(
		result, 
		data.frame( 
			woy = 10, dow = "Sun", hour = 3,
			count=NA
		)
	)
}else if(par$weighted){
	names(result)[4:6] <- c("count","triptime","dist")
	result <- rbind(
		result, 
		data.frame(
			woy = 10, dow = "Sun", hour = 3, 
			count = NA, triptime = NA, dist = NA
		)
	)
}else{
	names(result)[4:5] <- c("count","pace")
	result <- rbind(
		result, 
		data.frame(
			woy = 10, dow = "Sun", hour = 3, 
			count = NA, pace = NA
		)
	)
}
result$dow <- factor(
	result$dow, 
	levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
)


trellis.device(
	postscript, 
	file = "~/count.vs.hour.byweekday.ps", 
	color = TRUE,
	paper = "legal"
)
a <- xyplot( log2(count) ~ hour | factor(dow)*factor(woy),
	data = result,
	pch  = 16,
	layout = c(7,3),
	aspect = "xy",
	cex  = 0.5,
	xlab = list(label = "Hour of day"),
	ylab = list(label = "Log number of rides per hour (log base 2)"),
	panel = function(x,y,subscripts,...){
				panel.abline(v=5, col="red", lty=2, lwd=0.5)
      if (length(x)!=0) {
        if (unique(result[subscripts,]$dow) == "Tue"){
          panel.text(
            x = 22,
            y = 10.8, 
            paste(result[subscripts,]$month[1], result[subscripts,]$day[1], sep=""),
            cex = 0.8
          )
        }
      }
		panel.xyplot(x,y,...)
	}
)
print(a)
dev.off()

trellis.device(
	postscript, 
	file = "~/count.vs.week.bydayhour.ps", 
	color = TRUE,
	paper = "legal"
)
a <- xyplot( log2(count) ~ woy | factor(hour)*factor(dow),
	data = result,
	pch  = 16,
	layout = c(12,2),
	cex  = 0.5,
  key=list(
    text = list(label=c(
      "log hourly rides",
      "robust loess smoothing: span=0.75, degree=2 "
    )),
    lines = list(
      pch=16, 
      cex=0.7, 
      lwd=1.5, 
      type=c("p","l"), 
      col=col[1:2]
    ), 
    columns = 2
  ),	
	scale = list(x=list(tick.number=4)),
	xlab = list(label = "Week"),
	ylab = list(label = "Log number of rides per hour (log base 2)"),
	panel = function(x,y,...){
		panel.xyplot(x,y,...)
		panel.loess(x,y, degree=2, span=0.75, family = "symmetric", col=col[2],...)
	}
)
print(a)
dev.off()


mean.w <- ddply(
	.data = result,
	.variable = "woy",
	.fun = summarise,
	median = median(log2(count), na.rm=TRUE),
	mean = mean(log2(count), na.rm =TRUE)
)
trellis.device(
	postscript, 
	file = "~/week.mean.ps", 
	color = TRUE,
	paper = "legal"
)
a <- xyplot( mean ~ woy,
	data = mean.w,
	type = "b",
	pch  = 1,
	aspect = "xy",
	cex  = 1,
	xlab = list(label = "Week of year"),
	ylab = list(label = "Weekly mean of log rides per hour (log base 2)"),
)
print(a)
dev.off()

##############
##Trying    ##
##############
result <- result[with(result, order(woy, dow, hour)), ]
result$time <- 1:nrow(result)
trellis.device(
	postscript, 
	file = "~/count.vs.hour.ps", 
	color = TRUE,
	paper = "legal"
)
a <- xyplot( log2(count) ~ time/24,
	data = result,
	type = "l",
	lwd = 0.7,
	xlab = list(label = "Hour of day"),
	ylab = list(label = "Pace"),
	xlim = c(0, 365),
	scale = list(x=list(tick.number=40)),
	panel = function(x,y,...){
		panel.xyplot(x,y,...)
#		panel.abline(v=5, col="red", lty=2, lwd=0.5)
	}
)
print(a)
dev.off()
