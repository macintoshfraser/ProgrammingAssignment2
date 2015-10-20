## These functions are for Corsera, R Programming, Assignment 2
## The functions take a matrix then calculate and cache an inverse of the matrix.

## The makeCacheMatrix function contains the following sub-functions
##      *       set           sets the input matrix and resets the cache status (m) to "NULL" 
##      *       get           recalls the input matrix 
##      *       setinverse    runs the cacheSolve function and changes the cache status (m) from "NULL" to the inverse.
##      *       getinverse    recalls the solved matrix
## makeCacheMatrix also creates a list of results of the 4 sub-functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(cacheSolve) m <<- cacheSolve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function checkes to see if the cache status is null before performing the calculation.
##      If the cache status (m) is NOT "NULL", it retreives the result from the cache.
##      If the cache status (m) is "NULL, it gets the data input from "get" and calculates the inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}