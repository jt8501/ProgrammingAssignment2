## These functions are designed to increase speed and efficiency
## when calculating matrix inverses by caching the value
## of a matrix inverse once it has been computed.

## This function creates a special matrix object
##      that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        data.frame(set = set, get = get, 
                   setinverse = setinverse,
                   getinverse = getinverse)
}

## This function checks to see if a cached inverse exists.
##      If yes, return it.  Otherwise, calculate the inverse
##      of the matrix and return it.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
