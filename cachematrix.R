## This pair of functions creates a means for speeding up matrix operations  
## by storing previously-calculated inverses of matrices in a 'cache' environment.

## The first function defines the framework for a cached matrix, and 
## provides the means for storing previously calculated inverses.

makeCacheMatrix <- function(x = matrix()) {
 	z <- NULL
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) z <<- solve
        getinverse <- function() z
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function calls the first to check whether an inverse has
## already been calculated. If so, it prints a tell-tale message and 
## returns the cached value. Else it calculates the inverse and stores 
## the result in the cache.

cacheSolve <- function(x, ...) {
     	z <- x$getinverse()
        if(!is.null(z)) {
                message("getting cached inverse")
                return(z)
        }
        data <- x$get()
        z <- solve(data, ...)
        x$setinverse(z)
        z
}
