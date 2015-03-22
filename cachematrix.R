## Two methods in this file introduce caching for matrix inversion calculation


## Function makes a special object that can store a matrix and its inversion
## arguments:
##      x - matrix to store
## returns: wrapper objects with methods get(), set(), getInversion(), setInversion()
makeCacheMatrix <- function(x = matrix()) {
    # cached value
    inverse <- NULL
    
    # set source matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # get source matrix
    get <- function() x
    
    # set inverse for caching
    setIversion <- function(pInverse) inverse <<- pInverse
    
    # get cached inverse
    getInversion <- function() inverse
    
    # return 4 API methods
    list(set = set, 
         get = get,
         setIversion = setIversion,
         getInversion = getInversion)
}


## Function returns an inversion of given matrix
## arguments:
##      x - special object returned by makeCacheMatrix
##      ... - arguments for builtin solve() function
## returns: inverse of source matrix
cacheSolve <- function(x, ...) {
    # try to get cached value
    inv <- x$getInversion()
    
    # cached value exists
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # calc new inversion
    data <- x$get()
    inv <- solve(data, ...)
    
    # store in cache
    x$setIversion(inv)
    
    # return calced value
    inv
}
