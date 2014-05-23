## These functions will solve the inverse of an invertible matrix.
## If the inverse has already been computed it has been cached,
## and can be quickly retrieved instead of re-computing.

## makeCacheMatrix creates a speacial "matrix" that can cache
## its inverse. It returns a list.

makeCacheMatrix <- function(x = matrix()) {
        # inverted matrix has not been computed
        inv <- NULL           
        
        # store matrix, clear inverted matrix
        set <- function(y) {  
                x <<- y
                inv <<- NULL
        }
        
        # retrieve matrix
        get <- function() x   
        
        # store inverted matrix
        setinv <- function(invMatrix) inv <<- invMatrix
        
        # retrieve inverted matrix
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
  
}


## cacheSolve computes the inverse of the special matrix or
## retrieves the cached solution.

cacheSolve <- function(x, ...) {

        inv <- x$getinv()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # inverted matrix has not yet been computed
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
