## Assignment: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        ## Our special "matrix" object
        m <- NULL
    
        ## Define our get and set functions
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        
        ## Define set (cache) and get functions for inverse
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        ## Return vector of our functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

        ## Check if we have already computed the inverse
        m <- x$getinverse()
        
        ## If we have, return the cached inverse matrix
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        
        ## Otherwise compute the inverse and cache it
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        
        ## Return the matrix
        m
}
