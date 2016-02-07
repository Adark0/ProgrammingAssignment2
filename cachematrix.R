##########################################################
# The function makeCacheMatrix creates a list containing 
# 4 functions which
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. getthe value of the inverse
##########################################################

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y 
        m <<- NULL 
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


##########################################################
# The function cacheSolve returns the inverse of the matrix
# that was given above. However, it first checks to see if 
# the inverse has already been calculated. If so, it gets 
# the inverse from the cache and skips the computation. 
# Otherwise, it gets the value of the input matrix, computes the value of
# the inverse of the input matrix, runs the setinverse 
# function andreturns the inverse.
##########################################################

cacheSolve <- function(x, ...) {
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
