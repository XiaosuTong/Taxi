rst <- rhread("/ln/tongx/taxi/driver.cabcount")
df.plot <- get_quantile(rst[[1]][[2]]$cabs)
## use this to draw the quantiles
##  in panel function, check if the difference of two consecutive x values are greater than 
##  twice (or maybe 1.95x) of the minimum difference of all consecutive x values, if so, draw
##  a line to connect the two values, otherwise just plot the point
trellis.device(
  postscript, 
  file = "~/dist.cabperdirver.ps", 
  color = TRUE,
  paper = "legal"
)
a <- xyplot(log2(value) ~ x
  , data = df.plot
  , cex = 0.20
  , main = "Quantiles of Cab counts per Driver"
  , ylab = "log number of cabs one driver drove (log base 2)"
  , xlab = "f-value"
  , col = "blue"
  , panel = function(x,y,...) {
      x.diff <- diff(x)   # get the difference of all consecutive x values
      x.diff.min <- min(x.diff)    # get the minimum difference 
      index <- which(x.diff > 1.95 * x.diff.min)  # find where to draw lines
      for (i in index) {  ## connect the lines
        panel.segments(x[i],y[i],x[i+1],y[i+1],col="blue",lwd=1.75)
      }
      panel.abline(v=(0:10)/10,h=0:ceiling(max(y)), col='lightgrey', lty=3, lwd=0.5)
      panel.xyplot(x,y,...)
    }
)
print(a)
dev.off()

rst <- rhread("/ln/tongx/taxi/driver.dist")
result <- unlist(lapply(rst, "[[", 2))
df.plot <- get_quantile(result)
trellis.device(
  postscript, 
  file = "~/dist.distribution2.ps", 
  color = TRUE,
  paper = "legal"
)
a <- xyplot(log2(value) ~ x
  , data = df.plot
  , subset = x > 0.9999
  , cex = 0.20
  , main = "Quantiles of Distance per Rides"
  , ylab = "Log of Distance (log base 2 mile)"
  , xlab = "f-value"
  , col = "blue"
  , scale = 
  , panel = function(x,y,...) {
      x.diff <- diff(x)   # get the difference of all consecutive x values
      x.diff.min <- min(x.diff)    # get the minimum difference 
      index <- which(x.diff > 1.95 * x.diff.min)  # find where to draw lines
      for (i in index) {  ## connect the lines
        panel.segments(x[i],y[i],x[i+1],y[i+1],col="blue",lwd=1.75)
      }
      panel.abline(
        v = seq(0.99990,1,by=0.00001), h = seq(0,ceiling(max(y)),5), 
        col='lightgrey', lty=3, lwd=0.5
      )
      panel.xyplot(x,y,...)
      panel.abline(h=max(y), col="black", lty=2, lwd=0.5)
      panel.abline(v=max(x), col="black", lty=2, lwd=0.5)
      panel.text(0.9999, max(y), paste(round(max(y),2)))
      panel.text(x[which.max(y)], 0, round(max(x), 4), adj=c(0,0))
    }
)
print(a)
dev.off()


rst <- rhread("/ln/tongx/taxi/driver.countcounts")
result <- data.frame(
  matrix(unlist(lapply(rst, "[[", 2)), ncol=2, byrow=TRUE), 
  stringsAsFactors=FALSE
)
names(result) <- c("counts", "cabs")
trellis.device(
  postscript, 
  file = "~/counts.vs.cabs.ps", 
  color = TRUE,
  paper = "legal"
)
a <- xyplot( log2(counts) ~ log2(cabs)
  , data = result
  , cex = 0.20
  , main = "Rides vs. Cab Counts per Driver"
  , ylab = "Rides"
  , xlab = "Cab Counts"
  , col = "blue"
  , scale = 
  , panel = function(x,y,...) {
      panel.xyplot(x,y,...)
    }
)
print(a)
dev.off()