## R Programming
## Assigment 2
##
## This second programming assignment will require you to write an R function 
## is able to cache potentially time-consuming computations. In this particular 
## case we are going to cache the inverse of a given matrix.
##
## First step will be to define a special "matrix" that wraps the original one
## and provide methods to store the inverse of this matrix once is its calculated
## the first time
## 
## The secod function uses this special "matrix" as input parameter to return 
## the inverse of the matrix. Instead of do the calculation every time it is called
## this function first check if the result was already stored in the internal cache
## returning that value. Otherwise, do the actual calculations and store the 
## result in the internal matrix for its later use
##
## To check that code works do the following steps:
##
##
## 1. Create a matrix: 
##
## $ source("cachematrix.R")
## $ m <- matrix(1:4, 2, 2)
## $ m
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## 2. Create the special "matrix" bases on this one
##
## $ mcache <- makeCacheMatrix(m)
##
## 3. Calculate the inverse of this matrix usign the new cacheSolve function with the special "matrix"
##
## $ inv <- cacheSolve(mcache)
##
## (Note that no cached message is shown)
##
## 4. Ensure it is the inverse of the original one
##     
## $ inv
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## $ inv %*% m
##     [,1] [,2]
## [1,]    1    0
## [2,]    0    1
##
## 5. Finally, calculate the inverse again. You should see the cache message appear:
##
## $ inv <- cacheSolve(mcache)
## getting cached data




## This function creates a special "matrix" based on the actual matrix passed as argument
## Actually it returns a list of functions defined to refer to the original matrix (get/set)
## and the inverse (getinverse/setinverse), that is stored in the local variable "invmatrix"
## the first time it is calculated.
##
## Note that the "set" function is used only if you want to reuse the special "matrix" 
## with a new matrix, different from the one used when it was created:
## 
## $ mcache <- makeCacheMatrix(m1)
## ...
## $ mcache$set(m2)
## $ cacheSolve(mcache) 

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


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invmatrix <- x$getinverse()
    if(!is.null(invmatrix)) {
        message("getting cached data")
        return(invmatrix)
    }
    data <- x$get()
    invmatrix <- solve(data, ...)
    x$setinverse(invmatrix)
    invmatrix    
}
