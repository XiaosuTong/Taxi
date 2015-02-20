source("~/Rhipe/rhinitial.R")
lib.loc <- "/home/shaula/u16/tongx/R_LIBS"
input.dir <- "/ln/xdata-taxi/raw"
output.dir <- "/ln/tongx/taxi"
library(lattice)
lattice.theme <- trellis.par.get()
col <- lattice.theme$superpose.symbol$col
library(plyr)

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

##ploting##
result <- result[order(result$woy, result$dow, result$hour),]
result$time <- 1:nrow(result)

par <- list()
par$sw <- "periodic"
par$sd <- 1
par$tw <- 1665
par$td <- 1
par$inner <- 10
par$outer <- 5

rst <- stl2(log2(result$count), result$time, 
	n.p = 168,
  sub.start = 25, 
	s.window = par$sw, 
	s.degree = par$sd, 
	t.window = par$tw, 
	t.degree = par$td, 
	inner = par$inner, 
	outer = par$outer
)$data

result <- cbind(result, rst)
result$how <- (as.numeric(result$dow)-1)*24 + result$hour

## scatter plot of trend+seasonal vs. hour
trellis.device(
  device = postscript, 
  file = "~/trend+seasonal.vs.hour.ps",
  color=TRUE, 
  paper="legal"
)
  b <- xyplot( raw ~  hour | factor(dow)*factor(woy),
    data = result,
    xlab = list(label = "Hour"),
    ylab = list(label = "Log number of rides per hour (log base 2)"),
    layout = c(7,3),
		aspect= "xy",
		strip = strip.custom(var.name="Week"),
  	key = list(
    	text = list(label=c(
    		"log hourly rides",
      	"trend+seasonal component"
    	)),
    	lines = list(
      	lwd=1.5, 
      	cex=0.7,
      	pch=16,
      	type=c("p","l"), 
      	col=col[1:2]
    	), 
    	columns = 2
  	),
    scales = list(
      y = list(relation = 'same', alternating=TRUE)
#      x = list(at=seq(0, 24, by = 2), relation='same')
    ),
    panel = function(x,y,subscripts,...) {
#      panel.abline(v=seq(0,168, by = 24), color="lightgrey", lty=3, lwd=0.5)
      panel.xyplot(
      	x = x, 
      	y = y, 
      	type="p", col=col[1], pch=16, cex=0.5, ...
      )
			panel.xyplot(
				x = x, 
				y = (result[subscripts,]$trend+result[subscripts,]$seasonal), 
				type="l", col=col[2], lwd=1, ...
			)            
    }
  )
  print(b)
dev.off()

## QQ plot of remainder
trellis.device(
  device = postscript, 
  file = "~/QQ.remainder.ps",
  color=TRUE, 
  paper="legal"
)
	a <- qqmath(~ remainder,
		data = result,
		distribution = qnorm,
		aspect = 1,
		pch = 16,
		cex = 0.3,
		ylim = c(-3,3),
    xlab = list(label="Unit normal quantile"),
    ylab = list(label = "Log number of rides per hour (log base 2)"),
    prepanel = prepanel.qqmathline,
    panel = function(x, y,...) {
      panel.grid(lty=3, lwd=0.5, h=-1, v=-1, col="black",...)
      panel.qqmathline(x, y=x)
      panel.qqmath(x, y,...)
    }
	)
  print(a)
dev.off()

##scatter plot of remainder vs. hour
trellis.device(
  device = postscript, 
  file = "~/remainder.vs.hour.ps",
  color=TRUE, 
  paper="legal"
)
  b <- xyplot( remainder ~ hour | factor(dow)*factor(woy),
    data = result,
    xlab = list(label = "Hour"),
    ylab = list(label = "Log number of rides per hour (log base 2)"),
    type = "p",
    pch = 16,
    cex = 0.5,
    layout = c(7,3),
    scales = list(
    	y = list(relation = 'free')
			#x = list(at=seq(0, 168, by = 24), relation='same')
    ),
    panel = function(x,y,...) {
      panel.abline(h=0, lty=2, lwd = 0.5, col="red")
      panel.xyplot(x,y,...)
    }
  )
  print(b)
dev.off()

## Auto correlation ACF for the remainder
Acf <- acf(result$remainder, plot=FALSE)
ACF <- data.frame(
	correlation = Acf$acf,
	lag = Acf$lag
)
trellis.device(
  device = postscript, 
  file = "~/ACF.remainder.ps",
  color=TRUE, 
  paper="legal"
)
  b <- xyplot( correlation ~ lag,
  	data = ACF,
  	subset = lag != 0,
    xlab = list(label = "Lag"),
    ylab = list(label = "Acf"),
   	type = "h",
    panel = function(x,y,...) {
      panel.abline(h=0)
      panel.xyplot(x,y,...)
    }
  )
	print(b)
dev.off()

## trend component and weekly average of raw vs. hour
dr <- ddply(
  .data = result,
	.variables = "woy",
	.fun = summarise,
	mean = mean(raw)
)
row.c <- ddply(
	.data = result, 
	.variables = "woy", 
	.fun = function(r){nrow(r)}
)
mm <- dr[rep(c(1:53), times=row.c$V1), ]
rst.trend <- cbind(result, mean= mm$mean)

trellis.device(
    device = postscript, 
    file = "~/trend.vs.hour.ps", 
    color = TRUE, 
    paper = "letter"
)
	b <- xyplot( mean ~ time, 
    data = rst.trend,
    xlab = list(label = "Hour"),
    ylab = list(label = "Log number of rides per hour (log base 2)"),
    pch = 16,
		aspect = "xy",
    key=list(
      text = list(
       	label=c("trend component","weekly mean")
      ), 
      lines = list(
      	pch = 16, 
      	cex = 0.7, 
      	lwd = 1.5, 
      	type = c("l","p"), 
      	col = col[1:2]
      ),
      columns=2
    ),
    cex = 0.5,
    scales = list(
      y = list(relation = 'free') 
    ),
		prepanel = function(x,y,subscripts,...){
			v <- rst.trend[subscripts,]
			ylim <- range(v$mean)
			ans <- prepanel.default.xyplot(v$time, v$trend, ...)
			ans$ylim <- range(ans$ylim, ylim)
			ans
		},
    panel = function(x, y, subscripts, ...) {
      panel.xyplot(
        x = x[seq(84, 8760, by=168)],
        y = y[seq(84, 8760, by=168)],
        type="p", col=col[2], ...
      )
			panel.xyplot(
				x = rst.trend$time, 
				y = rst.trend$trend, 
				type="l", col=col[1], ...
			)
    }
	)
	print(b)
dev.off()

#######################
## conditional on month
#######################
##seasonal vs. week 
trellis.device(
    device = postscript, 
    file = "~/seasonal.vs.week.ps", 
    color = TRUE, 
    paper = "legal"
)
  b <- xyplot( seasonal ~ woy | as.factor(hour)*dow,
    data = result,
    xlab = list(label = "Week"),
    ylab = list(label = "Log number of rides per hour (log base 2)"),
    type = c("p"),
    pch=16,
    cex=0.5,
    layout = c(12,1),
    strip = TRUE,
    scales = list(
      y = list(relation = 'same', alternating=TRUE), 
      x=list(tick.number=4, relation='same')
    ),
    panel = function(x,y,...) {
      panel.abline(h=0, color="black", lty=1)
      panel.xyplot(x,y,...)
    }
  )
  print(b)
dev.off()

##remainder vs.week
trellis.device(
    device = postscript, 
    file = "~/remainder.vs.week.ps", 
    color = TRUE, 
    paper = "legal"
)
  b <- xyplot( remainder ~ woy | as.factor(hour)*dow,
    data = result,
    xlab = list(label = "Week"),
    ylab = list(label = "Log number of rides per hour (log base 2)"),    
    pch = 16,
    cex = 0.5,
    layout = c(12,1),
    strip = TRUE,
    key=list(
      text = list(
        label=c("remainder", "remainder ouside [-1.5, 1]", "loess smoothing: span =0.75, degree=2")
      ), 
      lines = list(
        pch = 16, 
        cex = 0.7, 
        lwd = 1.5, 
        type = c("p","p","l"), 
        col = c(col[1], "red", col[2])
      ),
      columns=3
    ),
    scales = list(
      y = list(relation = 'same', alternating=TRUE), 
      x=list(tick.number = 4, relation='same')
    ),
    ylim = c(-1.6, 1.1),
    panel = function(x,y,...){
      idx1 <- which(y < (-1.5))
      idx2 <- which(y > 1)
      idx3 <- which(y<1 & y> -1.5)
      panel.abline(h=0, color="black", lty=1)
      panel.xyplot(x[idx3],y[idx3], ...)
      panel.xyplot(x[idx1], rep(-1.5, length(idx1)), col = "red",...)
      panel.xyplot(x[idx2], rep(1, length(idx2)), col = "red", ...)
      panel.loess(x,y,span=3/4, degree=2, col=col[2],...)
    }
  )
  print(b)
dev.off()
trellis.device(
    device = postscript, 
    file = "~/remainder2.vs.week.ps", 
    color = TRUE, 
    paper = "legal"
)
  b <- xyplot( remainder ~ woy | as.factor(hour)*dow,
    data = result,
    xlab = list(label = "Week"),
    ylab = list(label = "Log number of rides per hour (log base 2)"),   
    type = "b",      
    pch = 16,
    cex = 0.5,
    layout = c(2,6),
    strip = TRUE,
    scales = list(
      y = list(relation = 'free', alternating=TRUE), 
      x = list(at=seq(1,53, by=4), relation='same')
    ),
    panel = function(x,y,...) {
      panel.abline(h=0, color="black", lty=1)
      panel.xyplot(x,y,...)
    }
  )
  print(b)
dev.off()

##QQ plot of remainder
Qrst <- ddply(
  .data = result,
  .variable = c("hour", "dow"),
  .fun = function(r) {
    r <- r[!is.na(r$remainder),]
    a <- sort(r$remainder)
    idx <- 1:length(a)
    f.value <- (idx - 0.5) / length(a)
    qnorm <- qnorm(f.value)
    value <- data.frame(
      remainder = a, 
      qnorm = qnorm
    )
  }
)
trellis.device(
    device = postscript, 
    file = "~/QQ.remainder.hour.ps", 
    color = TRUE, 
    paper = "legal"
)
  a <- xyplot(remainder ~ qnorm| as.factor(hour)*dow,
    data = Qrst,
    pch = 16,
    cex = 0.4,
    key=list(
      text = list(
        label=c("remainder", "remainder ouside [-1.5, 1]")
      ), 
      lines = list(
        pch = 16, 
        cex = 0.7, 
        lwd = 1.5, 
        type = c("p","p"), 
        col = c(col[1], "red")
      ),
      columns=2
    ),
    layout = c(12,2),
    ylim = c(-1.6, 1.1),
    xlab = list(label="Unit normal quantile"),
    ylab = list(label = "Log number of rides per hour (log base 2)"),
    panel = function(x, y, ...) {
      panel.abline(h=0, lty=2, lwd=0.5, col="red")
      idx1 <- which(y < (-1.5))
      idx2 <- which(y > 1)
      idx3 <- which(y<1 & y> -1.5)
      panel.xyplot(x[idx3],y[idx3],...)
      panel.xyplot(x[idx1], rep(-1.5, length(idx1)), col ="red",...)
      panel.xyplot(x[idx2], rep(1, length(idx2)), col ="red",...)
      panel.qqmathline(y,y=y,...)
    }
  )
  print(a)
dev.off()