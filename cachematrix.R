# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# Th function makeCacheMatrix creates a special "matrix", wich is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    #variable that will store the inverted matrix
    inv <- NULL
    #set funciotn for the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    #get function for the matrix
    get <- function() x
    
    #set function for the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    #get function for the inverse of the matrix
    getInverse <- function() inv
    
    #return the matrix and the new functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
    
    inv <- x$getInverse()
    
    #if the inverse is alredy stored, return the cached result
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    #if the inverse is not calculated
    data <- x$get()
    inv <- solve(data, ...)
    
    #chace the inverse
    x$setInverse(inv)
    
    #return the inverse
    inv
}