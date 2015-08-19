## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invmatrix <- NULL
    set <- function(y) {
        x <<- y
        invmatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invmatrix <<- inverse
    getinverse <- function() invmatrix
    list(set = set, get = get,
         getinverse = getinverse,
         setinverse = setinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invmatrix <- x$getinverse()
    if(!is.null(invmatrix)) {
        message("getting cached data")
        return(invmatrix)
    }
    data <- x$get()
    invmatrix <- mean(data, ...)
    x$setinverse(invmatrix)
    invmatrix    
}
