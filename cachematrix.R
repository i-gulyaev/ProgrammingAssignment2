## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
