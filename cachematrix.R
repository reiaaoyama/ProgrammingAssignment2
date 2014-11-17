####
# 1.  `makeCacheMatrix`: This function creates a special "matrix" object
# that can cache its inverse.
# 2.  `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

## makeCacheMatrix() - creates a special "matrix" object that can 
# cache its inverse
# There are multiple methods that can act on cacheMatrix:
# 1. makeCacheMatrix(matrix) - create a cacheMatrix
# 2. get() - cacheMatrix$get() - getting the matrix
# 3. getinv() - cacheMatrix$getinv() - direct way to get the
#    inverse matrix that is already cache in the cachMatrix.
#    The value will be NULL until cacheSolve() is performed on
#    cacheMatrix.  Hence, cacheMatrix$getinv() should not be used
#    cacheSolve() should be used instead.
# 4. cacheSolve() a method to get the inverse matrix of cacheMatrix
#    The first time that it is called, it will solve the inverse matrix
#    and store the value for later use.  Subsequence calls will use
#    the cache value and will print out a message "Getting Cache value"

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list( set = set, get = get,
              setinv = setinv,
              getinv = getinv )
}


## cacheSolve() - a function to computes the inverse.
#  If there is an inversed cached, then just return the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if( !is.null(inv) ) {
                message("Getting Cache value")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
