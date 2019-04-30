## The purpose of these functions is to store the inverses of matrices in the cache.
## This can be very useful if running the solve function takes too much time.
## These functions will search the cache for a saved inverse, before wasting
## time running another solve function on a matrix.

## This function creates a matrix object whose inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
    i <<-matrix()
    set <- function(y) {
        x<<-y
        i <<-matrix()
    }
    get<- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function searches the cache for an inverse. 
## If there is no inverse to be found, then it will compute the inverse.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!anyNA(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}
