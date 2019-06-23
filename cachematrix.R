## These are pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setrmatrix <- function(solve) x <<- solve
        getrmatrix <- function() m
        list(set = set, get = get,
             setrmatrix = setrmatrix,
             getrmatrix = getrmatrix)
        
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getrmatrix()
        if(!is.null(m)) {
                message("getting cathed data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setrmatrix(m)
        m
}
