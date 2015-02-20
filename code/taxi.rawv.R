library(lattice)
rst <- rhread("/ln/tongx/taxi/tmp")
result <- do.call("rbind", lapply(rst, "[[", 1))
result <- data.frame(result, stringsAsFactors=FALSE)
names(result) <- c("woy", "dow", "hour")
result$count <- unlist(lapply(rst, "[[", 2))
result$woy <- as.numeric(result$woy)
result$hour <- as.numeric(result$hour)
result <- rbind(result, data.frame(woy=10, dow="Sun", hour = 3, count=NA))
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
	panel = function(x,y,...){
		panel.xyplot(x,y,...)
		panel.abline(v=5, col="red", lty=2, lwd=0.5)
	}
)
print(a)
dev.off()

trellis.device(
	postscript, 
	file = "~/count.vs.week.bydayhour.free.ps", 
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
      "robust loess smoothing: span=0.85, degree=2 "
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
	scale = list(y=list(relation="free")),
	xlab = list(label = "Week"),
	ylab = list(label = "Log number of rides per hour (log base 2)"),
	panel = function(x,y,...){
		panel.xyplot(x,y,...)
		panel.loess(x,y, degree=2, span=0.85, family = "symmetric", col=col[2],...)
	}
)
print(a)
dev.off()


mean.w <- ddply(
	.data = result,
	.variable = "woy",
	.fun = summarise,
	mean = mean(count)
)
trellis.device(
	postscript, 
	file = "~/week.mean.ps", 
	color = TRUE,
	paper = "legal"
)
a <- xyplot( log2(mean) ~ woy,
	data = mean.w,
	type = "b",
	pch  = 1,
	aspect = "xy",
	cex  = 1,
	xlab = list(label = "Week of year"),
	ylab = list(label = "Log number of mean of rides per week (log base 2)"),
)
print(a)
dev.off()
