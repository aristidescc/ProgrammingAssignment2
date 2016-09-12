## Functions for caching inverse matrix calculation. This is solved with one 
## generator function which uses lexical scoping rules to store a cache for 
## inverse matrix. Before solving inverse matrix calculation, cache variable is 
## previously checked and if cache is different than NULL, cached value is 
## returned.

## Generator function for managing matrix values and inverse matrix cache.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## Function to solve inverse matrix calculation, while making use of caching
## features provided by generator function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached result")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv    
}
