## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## Your assignment is to write a pair of functions that cache the inverse of a 
## matrix.

## <<- operator can be used to assign a value to an object in an environment 
## that is different from the current environment.

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## set inverse to NULL initially
    inverse <- NULL
    
    ## set: sets matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    ## get: gets matrix
    get <- function() {
        x
    }
    
    ## setInverse: sets inverse
    setInverse <- function(inv) {
        inverse <<- inv
    }
    
    ## getInverse: gets inverse
    getInverse <- function() {
        inverse
    }
    
    ## returns a list of all the functions
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    ## set inverse to retrieved/cached inverse
    inverse <- x$getInverse()
    
    ## if inverse is not NULL (aka, inverse exists from cache), return cached
    ## inverse and exit function.
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## else (if inverse is Null/does not exist in cache)
    ## get matrix
    matrix <- x$get()
    
    ## use solve to get the inverted matrix
    inverse <- solve(matrix, ...)
    
    ## cache the newly inverted matrix
    x$setInverse(inverse)

    ## returns a matrix that is the inverse of x
    inverse
}
