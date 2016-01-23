## Put comments here that give an overall description of what your
## functions do

## Create a special matrix object which would cache the inverse

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
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return inverse of matrix created by makeCacheMatrix(), 
## return cached inverse value if available. Otherwise calculate
## the inverse value and store it in cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    message("cached data not found, calculate matrix inverse")
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
