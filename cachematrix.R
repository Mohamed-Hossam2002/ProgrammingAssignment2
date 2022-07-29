## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
# This function makes a special 2x2 matrix that contains functions to
# set,get matrix data and to set, store the inverse of the matrix making it act
# as a cache.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    setmatrix <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    getmatrix <- function() x
    
    setinverse <- function(inv) i <<- inv
    
    getinverse <- function() i
    
    matrix(data=c(setmatrix, getmatrix, setinverse, getinverse), nrow=2, ncol=2)
}


## Write a short comment describing this function
# This function retrieves the inverse of the special matrix if present
# if it wasn't computed then it computes it and stores it in the special matrix
# cache.

cacheSolve <- function(x, ...) {
    
    inv <- x[2,2][[1]]() # retrieve the inverse data from the special matrix
    if (!is.null(inv)){
        message('getting cached matrix inverse')
        return(inv)
    }
    
    matrixdata <- x[1,2][[1]] # retrieves the matrix data from the special matrix
    
    inv <- solve(matrixdata, ...)
    
    x[1,2][[1]](inv) # sets the computed inverse in the cache of the special matrix
    
    inv
}
