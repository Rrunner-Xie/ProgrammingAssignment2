## The following two functions work together can create the inverse of a matrix and cache the inverse
## to allow you retrieve the inverse from the cache directly without repeat computation

## Create and cache a list of functions to set and get value of matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
    invmtx <- NULL
    setmtx <- function(y){
        x <<- y
        invmtx <<- NULL
    }
    getmtx <- function() x
    setinvmtx <- function(z) invmtx <- z
    getinvmtx <- function() invmtx
    list(setmtx = setmtx, getmtx = getmtx, setinvmtx = setinvmtx, getinvmtx = getinvmtx)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## Or, retrieve the inverse, which have been calculated, from the cache.

cacheSolve <- function(x, ...) {
    invmtx <- x$getinvmtx()
    if(!is.null(invmtx)){
        message("getting cached data")
        return(invmtx)
    }
    data <- x$getmtx()
    invmtx <- solve(data, ...)
    x$setinvmtx(invmtx)
    invmtx        ## Return a matrix that is the inverse of 'x'
}
