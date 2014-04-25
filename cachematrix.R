## This is the implementation of the peer-assignment of R Programming course
## Contrains 2 functions- first, makeCacheMatrix to create a "special" matrix 
## to be used to solve a matrix with caching the result. Second function is 
## cacheSolve used to solve a "special" matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {	
        r <- NULL
        set <- function(y) {
                x <<- y
                r <<- NULL
        }
        get <- function() x
        setreverse <- function(reversed) r <<- reversed
        getreverse <- function() r
        list(set = set, get = get,
             setreverse = setreverse,
             getreverse = getreverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        r <- x$getreverse()
        if(!is.null(r)) {
                message("getting cached data")
                return(r)
        }
        data <- x$get()
        r <- solve(data, ...)
        x$setreverse(r)
        r
}

