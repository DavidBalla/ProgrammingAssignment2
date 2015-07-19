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

cacheSolve <- function(x, ...) {
    ## @x: output of makeCacheMatrix()
    ## return: inverse of the original matrix input to makeCacheMatrix()
    
    inv = x$getinv()
    
    # if the inverse has already been calculated
    if (!is.null(inv)){
        # get it from the cache and skips the computation. 
        message("getting cached data")
        return(inv)
    }
    
    # otherwise, calculates the inverse 
    mat.data = x$get()
    inv = solve(mat.data, ...)
    
    # sets the value of the inverse in the cache via the setinv function.
    x$setinv(inv)
    
    return(inv)
}

test = function(mat){
    ## @mat: an invertible matrix
    
    temp = makeCacheMatrix(mat)
    
    start.time = Sys.time()
    cacheSolve(temp)
    dur = Sys.time() - start.time
    print(dur)
    
    start.time = Sys.time()
    cacheSolve(temp)
    dur = Sys.time() - start.time
    print(dur)
}


set.seed(1110201)
r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)
test(mat1)
