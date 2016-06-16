## one function takes a matrix and caches its inverse. the second function
## will pull the cached inverse instead of solving it independently.

## this function takes an invertible matrix as its input and caches the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse 
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## this function computers the inverse of the matrix created in makeCacheMatrix.
## if the inverse has already been calculuated, it will pull from cache

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    return(inv)
}
