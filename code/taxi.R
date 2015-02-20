source("~/Rhipe/rhinitial.R")
lib.loc <- "/home/shaula/u16/tongx/R_LIBS"
input.dir <- "/ln/xdata-taxi/raw"
output.dir <- "/ln/tongx/taxi"
#input.dir <- "/ln/tongx/taxi/tmp.csv"

job <- list()
job$map <- expression({
	lapply(seq_along(map.values), function(r) {
		line = strsplit(map.values[[r]], ",")[[1]]
    pick = as.numeric(unlist(strsplit(line[6],"[- :]")))
#   drop = as.numeric(unlist(strsplit(line[7],"[- :]")))
    dow = weekdays(as.Date(line[6]), abbreviate = TRUE)
    woy = week((as.Date(line[6])))
    hour = as.numeric(pick[4]) + 1
    rhcollect(c(woy, dow, hour), 1)
	})
})
job$reduce <- expression(
  pre = {
    count <- 0
  },
  reduce = {
    count <- count + sum(unlist(reduce.values))
  },
  post = {
    rhcollect(reduce.key, count)
  }
)
job$setup <- expression(
	map = {
	  library(lubridate, lib.loc = lib.loc)
	}
)
job$input <- rhfmt(
	file.path(input.dir),
	type = "text"
) 
job$output <- rhfmt(
	file.path(output.dir, "tmp"),
	type = "sequence"
)
job$mapred <- list(
	mapred.reduce.tasks = 72, 
	rhipe_reduce_buff_size = 10000
)
job$mon.sec <- 5
job$jobname <- file.path(output.dir, "hourcounts")
job$readback <- FALSE
job$combiner <- TRUE
job.mr <- do.call("rhwatch", job)

## cacluate the pace of each hour
weighted <- FALSE
if(weighted){
  par <- "hour.wpacecounts"
}else{
  par <- "hour.pacecounts"  
}
job <- list()
job$map <- expression({
  lapply(seq_along(map.values), function(r){
    line <- strsplit(map.values[[r]], ",")[[1]]
    pick <- as.numeric(unlist(strsplit(line[6],"[- :]")))
    drop <- as.numeric(unlist(strsplit(line[7],"[- :]")))
    dow <- weekdays(
      x = as.Date(line[6]), 
      abbreviate = TRUE
    )
    woy <- week((as.Date(line[6])))
    hour <- as.numeric(pick[4]) + 1
    dist <- as.numeric(line[10])
    picktime <- as.POSIXct(
      x = line[6], 
      format = "%Y-%m-%d %H:%M:%S", 
      tz = "EDT"
    )
    droptime <- as.POSIXct(
      x = line[7], 
      format = "%Y-%m-%d %H:%M:%S", 
      tz = "EDT"
    )
    triptime <- as.numeric(
      difftime(droptime, picktime, units = "hours")
    )
    if(weighted) { 
      rhcollect(c(woy, dow, hour), c(1, triptime, dist))
    }else{
      rhcollect(c(woy, dow, hour), c(1, pace=triptime/dist))
    }   
  })
})
job$reduce <- expression(
  pre = {
    count <- 0
    triptime <- 0
    value <- vector()
    pace <- 0
  },
  reduce = {
    value <- c(value, unlist(reduce.values))
  },
  post = {
    if(weighted) {
      count <- sum(value[seq(1, length(value), by=3)])
      dist <- sum(value[seq(3, length(value), by=3)])
      triptime <- sum(value[seq(2, length(value), by=3)])
      rhcollect(reduce.key, c(count, dist, triptime))
    }else{
      count <- sum(value[seq(1, length(value), by=2)])
      pace <- sum(value[seq(2, length(value), by=2)])
      rhcollect()
    }
  }
)
job$setup <- expression(
  map = {
    library(lubridate, lib.loc = lib.loc)
  }
)
job$parameters <- list(
  weighted = weighted
)
job$input <- rhfmt(
  file.path(input.dir),
  type = "text"
) 
job$output <- rhfmt(
  file.path(output.dir, par),
  type = "sequence"
)
job$mapred <- list(
  mapred.reduce.tasks = 72,
  rhipe_reduce_buff_size = 10000
)
job$mon.sec <- 5
job$jobname <- file.path(output.dir, par)
job$readback <- FALSE
job$combiner <- TRUE
job.mr <- do.call("rhwatch", job)
