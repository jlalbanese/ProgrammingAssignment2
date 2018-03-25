## makeCacheMatrix defines a special matrix capable of caching its inverse and 
## then cacheSolve retrieves the inverse if possible and calculates it otherwise

## creates a matrix with a cacheable inverse with get and set functions

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## Retrieves the cached inverse if available, otherwise calculates and caches it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    i <- solve(x$get(), ...)
    x$setinverse(i)
    i
}
