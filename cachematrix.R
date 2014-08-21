## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        SetMatrix <- function (y) {
                x <<- y
                #Setting m to NULL if the matrix has changed
                inv <<- NULL
        }
        GetMatrix <- function() x
        SetInverse <- function(inverse) inv <<- inverse
        GetInverse <- function() inv
        list(SetMatrix = SetMatrix,GetMatrix = GetMatrix,
             SetInverse = SetInverse,GetInverse = GetInverse)
}


#This function computes the inverse of the"matrix" or retrive it from cache

cacheSolve <- function(x, ...) {
	inv <- x$GetInverse()
        #Checking cahed data for inverse
        if(!is.null(inv)){
                message("Getting cached data")
                return(inv)
        }
        else {
                #retrieving new matrix        
                data <- x$GetMatrix()
                inv <- solve(data, ...)
                #Passing the inverse value to makeCacheMatrix
                x$SetInverse(inv)
                return(inv)
        }
}
