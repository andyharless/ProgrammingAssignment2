## When a matrix is inverted, cache the inverse 
## so inverse can be re-used without repeating the inversion

## makeCacheMatrix: Create an object that wraps a matrix and can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    cached_inverse = NULL
    set <- function(matrix_to_wrap) {
        x <<- matrix_to_wrap
        cached_inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inverse_to_be_cached) cached_inverse <<- inverse_to_be_cached
    getinv <- function() cached_inverse
    list( set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve: Get inverse of matrix wrapped in an object returned by makeCacheMatrix.
##   If cache already exists, cacheSolve will retrieve the inverse from cache.
##   If no cache exists, cacheSolve will do the inversion and cache the result.

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of the matrix wrapped by argument x
    the_inverse = x$getinv()
    if (!is.null(the_inverse)) {
        message("Cached inverse retrieved.")
        return(the_inverse) 
    }
    the_original_matrix = x$get()
    the_inverse = solve(the_original_matrix, ...)
    x$setinv(the_inverse)
    the_inverse
}
