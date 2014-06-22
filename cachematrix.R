# Function makeCacheMatrix and cacheSolve are used in combination.
# makeCacheMatrix creates a cache object for a matrix and its inverse matrix.
# Function cacheSolve takes a makeCacheMatrix object and computes the inverse of the cached matrix and store the result as a cache in the makeCacheMatrix object.
# If there is already a cache for the inverse matrix function cacheSolve only returns the cache.
# Example:
# mx <- matrix(11:14,2,2)
# cmx <- makeCacheMatrix(mx)
# inv <- cacheSolve(cmx)

# Function makeCacheMatrix creates a list consisting of the following four functions:
#   set        : set the cached value of a square matrix
#   get        : get the cached square matrix
#   setInverse : set the cached value of the inverse of the cached matrix
#   getInverse : get the cached value of the inverse of the cached matrix
#
# These functions have access to mx and inv in the enclosing environment.
#
makeCacheMatrix <- function(mx = matrix()) {
    inv <- NULL
    set <- function(mtx) {
        mx <<- mtx
        inv <<- NULL
    }
    get <- function() mx
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# Function cacheSolve calculates the inverse matrix of the cached matrix
# within the object created by the function makeCacheMatrix.
# The first time it is called it computes the inverse of the matrix by solve()
# and place the resulting inverse matrix in the makeCacheMatrix object.
# Subsequent calls to this function looks up the cached value quickly
# for the inverse of the matrix without the need to compute the inverse again.
#
cacheSolve <- function(cmx, ...) {
    inv <- cmx$getInverse()
    if (!is.null(inv)) {
        return(inv)
    }
    mx <- cmx$get()
    inv <- solve(mx, ...)
    cmx$setInverse(inv)
    # return a matrix that is the inverse of mx
    inv
}
