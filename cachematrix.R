## Issueing makeCacheMatrix prepares the cache for inverting a certain matrix. Running
## cacheSolve afterwards will either calculate the inverse or return the cached inverse
## matrix, if it has been calculated once already.

## makeCacheMatrix returns a list of functions to get/set data and the corresponding
## inverse matrix. Input parameters: the matrix to prepare caching for.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse matrix of its first input argument.
## If the result has been calculated already, it will get the cached version.
## Input parameters: the matrix to calculate the inverse for. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
