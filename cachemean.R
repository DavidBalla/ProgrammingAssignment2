## cachemean is designed to work with makeVector by takes a caching Vector 
## that was retuened by makeVector
cachemean <- function(x, ...) {   ## Starting cachemean
    m <- x$getmean()    ## get the mean of the vector x, $getmean return by makeVector
    if(!is.null(m)) {  ## is the mean chache?
        message("getting cached data")
        return(m)      ## returning the chached mean
    }
    data <- x$get()    ## call get to get the vector
    m <- mean(data, ...)  ## caculating the mean to m
    x$setmean(m)         ## chache the mean
    m                   ## return the mean
}