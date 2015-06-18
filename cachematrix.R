## Module provides set of functions for creating a 'matrix' object
## and computing its inverse in optimal way caching the result of computation
## in a 'matrix' object.

## makeCacheMatrix function constructs a 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    set <- function(y) {
        if (!identical(x, y)) {
            x <<- y
            matrixInverse <<- NULL
        }
    }
    get <- function() x
    setMatrixInverse <- function(i) matrixInverse <<- i
    getMatrixInverse <- function() matrixInverse
         list(set = set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
}


## cacheSolve function compute the inverse of a given 'matrix' object.
## If the inverse of a 'matrix' object has been already calculated,
## cached value of the inverse is returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrixInverse <- x$getMatrixInverse()
    if (!is.null(matrixInverse)) {
        message("getting cached matrix inversion")
        return(matrixInverse)
    }
    m <- x$get()
    matrixInverse <- solve(m, ...)
    x$setMatrixInverse(matrixInverse)
    matrixInverse
}
