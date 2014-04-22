## Function in the file provide a simple API for caching matrix inversion
## result.
## Matrix inversion operation is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute it
## repeatedly.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(mean) inv <<- mean
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## `makeCacheMatrix` above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
        xinv <- x$getinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        m <- x$get()
        xinv <- solve(m)
        x$setinv(xinv)
        xinv
}
