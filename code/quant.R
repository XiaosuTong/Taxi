###########################################
## function to get quantiles of count data
##  Assumes that for count data, 
##      there are a smaller number of values with a large number of replicates.
##      for values with few replicates, keep the values and make count=1
##      for values with large replicates, keep two copies of the values 
##          and make the first count=1 and second count=(actual.count - 1)
##  input:
##      - value: actual values, then count=NULL; 
##              or unique values, then count needs to be provided
##      - count: count of unique values, it should be a vector
##      - nq:    number of quantiles to produce
##  output: 
##      - a data.frame with value and x
###########################################
get_quantile <- function(value, count=NULL, nq=5000) {
    # total number of values
    total.count <- if (is.null(count)) length(value) else sum(count)

    if (total.count <= nq) {    # not too many values

        # sort the values to make plotting easier
        value <- sort(value)
        x <- ((1:total.count) - 0.5) / total.count

    } else {    # too many values

        # get the unique values and counts if not supplied as input
        if (is.null(count)) {   
            v.tb <- table(value)
            value <- as.numeric(names(v.tb))
            count <- as.numeric(v.tb)
        }
        # find popular values that will be drawn as a line segment
        ord <- order(count)
        tmp.value <- value[ord]
        tmp.count <- count[ord]
        pop <- cumsum(tmp.count) > nq
        # duplicate popular values once with count=1
        value <- rep(tmp.value[which(pop)], times=2)
        count <- c(rep(1, times=sum(pop)), tmp.count[which(pop)] - 1)
        # duplicate other values by its count 
        value <- c(value, rep(tmp.value[which(!pop)], times=tmp.count[which(!pop)]))
        count <- c(count, rep(1, times=sum(tmp.count[which(!pop)])))
        # re-order by value then by count
        ord <- order(value, count)
        value <- value[ord]
        count <- count[ord]
        # compute f-value
        x <- (cumsum(count) - 0.5) / total.count

    }

    # return a data.frame of value and x
    data.frame(value = value, x = x)
}

get_quantile.sample <- function(value, nq=1000) {

  value <- value[!is.na(value)]
  value <- sort(value)
  idx <- round(seq(1, length(value), length.out = nq))
  f.value <- (idx - 0.5) / length(value)
  
  data.frame(value = value[idx], fv = f.value)
  
}
