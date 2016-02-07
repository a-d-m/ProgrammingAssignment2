## Put comments here that give an overall description of what your
## functions do

## Entering a comment to notate the severe disconnect between
## the material studied, depth of swirl exercises, and the actual
## assignments expected to be completed. This should also break up
## the monotony for the viewers of the Week 3 assignment.

## Creates a special matrix populated via a list
## using 'set' and 'get' to enter and retrieve values

makeCacheMatrix <- function(x = matrix()) {
        inv_mtrx <- NULL
        set <- function(y) {
                x <<- y
                inv_mtrx <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) inv_mtrx <<- inverse
        getInverse <- function() inv_mtrx
        
        list(
                set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## Calculates the inverse of matrix created in the makeCacheMatrix
## function.

cacheSolve <- function(x, ...) {
        inv_mtrx <- x$getInverse()
        
        if (!is.null(inv_mtrx)) {
                message("getting cached data")
                return(inv_mtrx)
        }
        
        mtrx <- x$get()
        inv_mtrx <- solve(mtrx, ...)
        x$setInverse(inv_mtrx)
        inv_mtrx
}
