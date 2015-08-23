########################################
## Get cab counts (medallion id counts) for each driver
##  dirver:
##   - key is hack_license
##   - value is a vector with all medallion id's
##  driver.cabcount:
##   - key is hack_license
##   - value is cabs counts drove by the driver
##  spetialdriver.RData:
##   - is a data.frame includes all drivers who drove more than 128 cabs in total
##  driver.dist:
##   - key is hack_license
##   - value is a vector with all rides' distance

source("~/Rhipe/rhinitial.R")
source("~/Projects/Spatial/NCAR/myloess/greatcircle.R")
lib.loc <- "/home/shaula/u16/tongx/R_LIBS"
output.dir <- "/ln/tongx/taxi"
if (test) {
  input.dir <- "/ln/tongx/taxi/tmp.csv"
} else {
  input.dir <- "/ln/xdata-taxi/raw"
}
################
###  dirver  ###
################
job <- list()
job$map <- expression({
  lapply(seq_along(map.values), function(r){
    line <- strsplit(map.values[[r]], ",")[[1]]      
    medallion <- line[1]
    hack_license <- line[2]
    rhcollect(hack_license, medallion)
  })
})
job$reduce <- expression(
  pre = {
    combine <- vector()
  },
  reduce = {
    combine <- c(combine, do.call("c", reduce.values))
  },
  post = {
    rhcollect(reduce.key, combine)
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
  file.path(output.dir, "driver"),
  type = "sequence"
)
job$mapred <- list(
  mapred.reduce.tasks = 72, 
  rhipe_reduce_buff_size = 10000
)
job$mon.sec <- 5
job$jobname <- file.path(output.dir, "driver")
job$readback <- FALSE
job$combiner <- TRUE
job.mr <- do.call("rhwatch", job)

#########################
###  driver.cabcount  ###
#########################
job <- list()
job$map <- expression({
	lapply(seq_along(map.values), function(r){
    value <- data.frame(
      id = map.keys[[r]], 
      cabs = length(unique(map.values[[r]])),
      stringsAsFactors = FALSE
    )
		rhcollect(1, value)
	})
})
job$reduce <- expression(
  pre = {
    combine <- data.frame()
  },
  reduce = {
    combine <- rbind(combine, do.call("rbind", reduce.values))
  },
  post = {
    rhcollect(reduce.key, combine)
  }
)
job$setup <- expression(
  map = {
    library(lubridate, lib.loc = lib.loc)
  }
)
job$input <- rhfmt(
  file.path(output.dir, "driver"),
  type = "sequence"
) 
job$output <- rhfmt(
  file.path(output.dir, "driver.cabcount"),
  type = "sequence"
)
job$mapred <- list(
  mapred.reduce.tasks = 1, 
  rhipe_reduce_buff_size = 10000
)
job$mon.sec <- 5
job$jobname <- file.path(output.dir, "driver.cabcount")
job$readback <- FALSE
job$combiner <- TRUE
job.mr <- do.call("rhwatch", job)

#####################
###  driver.dist  ###
#####################
job <- list()
job$map <- expression({
  lapply(seq_along(map.values), function(r){
    line <- strsplit(map.values[[r]], ",")[[1]]    
    hack_license <- line[2]
    dist <- as.numeric(line[10])
#    plat <- as.numeric(line[12])
#    plon <- as.numeric(line[11])
#    dlat <- as.numeric(line[14])
#    dlon <- as.numeric(line[13])
#    tmp.dist <- rdist.earth(
#      x1 = as.matrix(cbind(plon, plat)), 
#      x2 = as.matrix(cbind(dlon, dlat))
#    )
#    value <- data.frame(
#      picktime = picktime,
#      droptime = droptime,
#      plon = plon,
#      plat = plat,
#      dlon = dlon,
#      dlat = dlat,
#      car.id = medallion,
#      dist = dist,
#      sdist = as.vector(tmp.dist)
#    )
    rhcollect(hack_license, dist)
  })
})
job$reduce <- expression(
  pre = {
#    combine <- data.frame()
    combine <- vector()
  },
  reduce = {
#    combine <- rbind(combine, do.call(rbind, reduce.values))
    combine <- c(combine, unlist(reduce.values))
  },
  post = {
#    combine$droptime <- as.POSIXct(
#      x = combine$droptime, 
#      format = "%Y-%m-%d %H:%M:%S", 
#      tz = "EDT"
#    )
#    combine$picktime <- as.POSIXct(
#      x = combine$picktime, 
#      format = "%Y-%m-%d %H:%M:%S", 
#      tz = "EDT"
#    )
    rhcollect(reduce.key, combine)
  }
)
job$setup <- expression(
  map = {
    library(lubridate, lib.loc = lib.loc)
    load("spetialdriver.RData")
  }
)
job$parameters <- list(
  get_quantile = get_quantile
)
job$shared <- c(
  file.path(
    output.dir, "RData", "spetialdriver.RData"
  )
)
job$input <- rhfmt(
  file.path(input.dir),
  type = "text"
) 
job$output <- rhfmt(
  file.path(output.dir, "driver.dist"),
  type = "sequence"
)
job$mapred <- list(
  mapred.reduce.tasks = 72, 
  mapred.task.timeout = 800000,
  rhipe_reduce_buff_size = 10000
)
job$mon.sec <- 5
job$jobname <- file.path(output.dir, "driver.dist")
job$readback <- FALSE
job$combiner <- TRUE
job.mr <- do.call("rhwatch", job)

############################
###  driver.countcounts  ###
############################
job <- list()
job$map <- expression({
  lapply(seq_along(map.values), function(r){
    line <- strsplit(map.values[[r]], ",")[[1]]    
    hack_license <- line[2]
    dist <- as.numeric(line[10])
    value <- data.frame(
      rides = 1,
      cabs = line[1]
    )
    rhcollect(hack_license, value)
  })
})
job$reduce <- expression(
  pre = {
    combine <- data.frame()
  },
  reduce = {
    combine <- rbind(combine, do.call(rbind, reduce.values))
  },
  post = {
    rhcollect(reduce.key, c(sum(combine$rides), length(unique(combine$cabs))))
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
  file.path(output.dir, "driver.countcounts"),
  type = "sequence"
)
job$mapred <- list(
  mapred.reduce.tasks = 72, 
  mapred.task.timeout = 800000,
  rhipe_reduce_buff_size = 10000
)
job$mon.sec <- 5
job$jobname <- file.path(output.dir, "driver.countcounts")
job$readback <- FALSE
job.mr <- do.call("rhwatch", job)