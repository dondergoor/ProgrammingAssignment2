# these two functions (makeCacheMatrix and cacheSolve)
# together cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  # creates a special matrix object that can cache its inverse
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(y) inv <<- y
    getinverse <- function() inv
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

cacheSolve <- function(x, ...) {
  # returns a matrix that is the inverse of 'x'
  # using a makeCacheMatrix object as its input
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
