## makeVector takes a numeric vector, saved in the private variable x
## Returns a list of functions
makeVector <- function(x = numeric()) {
    m <- NULL                            ## Initializes the mean to Null
    set <- function(y) {                 ## Sets y vector to x vector and NULLS the mean 
                                         ##     outside of this enviroment
        x <<- y
        m <<- NULL
    }
    get <- function() x                  ## returns vector x
    setmean <- function(mean) m <<- mean ## sets the mean to m
    getmean <- function() m              ##  returns m which is the mean
    list(set = set, get = get,           ## list is the special vector containing 
                                         ##   the functions
         setmean = setmean,              ## from above
         getmean = getmean)
}
## Returning a list of functions