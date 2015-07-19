## This function creates a special "matrix" object that can cache its inverse.
## Returns a list of functions, set/get matrix set/get inverse
## this list will be used as the input to cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
    ## initializing the inverse to NULL 
    inv = NULL
    set = function(y) {
        
        ## set y to x in a different enviroment 
        x <<- y
        inv <<- NULL
    }
    
    get = function() x   ## returns x
    setinv = function(inverse) inv <<- inverse  ## sets the inverse 
    getinv = function() inv  ## returns the inv
    
    ##return the list of functions        
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}






## cacheSolve received the output of makeCacheMatrix
## returns the inverse of the original matrix input to makeCacheMatrix
cacheSolve <- function(x, ...) {
   
## get the inverse of the matrix x, $getinv from makeCacheMatrix    
    inv = x$getinv()
    
## Has the inverse already been calculated
    if (!is.null(inv)){
## if so, retreive it from the cache and skip the computation. 
        message("getting cached data")
        return(inv)
    }
    
## otherwise calculate the inverse 
    matx.data = x$get()
    message("caculating the inverse non-chached data")
    inv = solve(matx.data, ...)
    
    # sets the value of the inverse in the cache via the setinv function.
    x$setinv(inv)
    
    return(inv)
}
