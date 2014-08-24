## These functions handles a special matrix with caching functionality

## This function creates a special matrix that set or get the matrix and set or get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 	  i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function return the inverse of the matrix from cache and if it is not already calculated in cache ,it calculates it and store in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	  i <- x$getInverse()
        if(!is.null(i)) {
                message("getting Inverse ")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
