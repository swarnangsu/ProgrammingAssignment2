## FileName       : cachematrix.R
## Functions      : 
##                : 1. makeCacheMatrix
##                : 2. cacheSolve
## Functionality : Efficient Matrix inverse utility function by caching the 
##                  inverse to avoid recomputation everytime


## Utility function which takes a matrix as an input and provides setters,
## getters, and its inverse with caching

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    
    set <- function(y) {
        x <<- y
        ## Need to invalidate the cache in the setter
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xinv <<- inv
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    

}


## returns the inverse of the matrix.
## uses the cached value of the inverse whenever possible
## otherwise recomputes inverse and updates the cache
## Assumption : Input Matrix is invertible
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    dataX <- x$get()
    xinv <- solve(dataX, ...)
    x$setinv(xinv)
    xinv    
}
