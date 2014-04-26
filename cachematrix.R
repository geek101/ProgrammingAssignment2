## Since matrix inversion calculation is costly the following
## functions help with caching the matrix once calculated and
## only re-calculate if any element changes.

## makeCacheMatrix returns list of methods given a matrix
## as argument
## methods:
## a. setmatrix(m) - Set the new matrix and invalidate the inverse
## b. setxy(value, row, column) - Set the new value in matrix
##                              and invalidate the inverse
## c. getmatrix() - Return the matrix object
## d. setinv(inv) - Cache the inverse
## e. getinv()    - Return the cached inverse, could be NULL
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmatrix <- function(m) {
        ## replace the matrix with new one
        ## and invalidate the saved matrix.
        x <<- m
        inv <<- NULL
    }

    setxy <- function(v, i, j) {
        ## Helper to change a value in the matrix.
        ## Invalidates the saved inverse.
        x[i,j] <<- v
        inv <<- NULL
    }

    getmatrix <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv

    # Return the list of supported methods.
    list(setmatrix = setmatrix, setxy = setxy,
         getmatrix = getmatrix,
         setinv = setinv, getinv = getinv)
}


## cacheSolve returns the matrix inverse given the object
## returned by makeCacheMatrix.
## Algorithm:
## Given a makeCacheMatrix object check if the inverse returned
## by object$getinv() is NULL. If it is NULL then calculate new
## inverse and then save and return.
## If the value is not NULL then we have a cached inverse so
## return it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        # Return the cached inverse
        return(inv)
    }

    # No cached inverse so calculate, save and return
    m <- x$getmatrix()
    inv <- solve(m)
    x$setinv(inv)
    return(inv)
}
