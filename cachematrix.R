## Functions that can cache an compute the inverse of a "matrix"

## Function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        setInverse <- function(inverse) m <<- inverse
        
        getInverse <- function() m
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}


## Function to compute the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(daya, ...)
        
        x$setInverse(m)
        
        m
}
## Return a matrix that is the inverse of 'x'