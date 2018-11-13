## Implementation of functions makeCacheMatrix() and cacheSolve() which
## together compute an inverse of the matrix and cache it, so, that the matrix inverse
## can be re-used

## makeCacheMatrix: creates a special “matrix” object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setm <- function(y) {
                x <<- y
                m <<- NULL
        }
        getm <- function() x
        setm.inv <- function(givenm.inv) m <<- givenm.inv
        getm.inv <- function() m
        
        ## return the list of functions for the matrix.
        list(setm = setma, getm = getm,
             setm.inv = setm.inv,
             getm.inv = getm.inv)       
}

## cacheSolve: computes the inverse of the special “matrix” returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
m <- x$getm.inv()
        if(!is.null(m)) {
                message("getting cached matrix inverse")
                return(m)
        }
## Cached value not present so calculating the inverse and caching
        data <- x$getm()
        m <- solve(data, ...)
        x$setm.inv(m)
        m
}
