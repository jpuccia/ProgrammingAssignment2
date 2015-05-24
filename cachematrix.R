## The functions below are used to create a special "matrix" data object (makeCacheMatrix)
##... to store and access a matrix and its inverse and to calculate the inverse of a matrix (cacheSolve)
##... using the special "matrix" as input
##

## makeCacheMatrix
##
## Create a data object that will store a matrix and its inverse.
## Parameters:
##      x: A matrix
##
## Returns:
##      A list that contains a setter and getter (set(y), get()) for the matrix parameter
##      and a setter and getter (setinv(matinv), getinv()) for the inverse of the matrix parameter.
##
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse matrix to NULL
        inv <- NULL
        
        ## Setter for the input matrix.
        ## Each time this is set clear the inverse matrix
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        ## Getter for the input matrix
        get <- function() x
        
        ## Setter for the inverse matrix
        setinv <- function(matinv) inv <<- matinv
        
        ## Getter for the inverse matrix
        getinv <- function() inv
        
        ## Return a list of setters and getters for the source and inverse matricies
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve
##
## Returns the inverse of the a given matrix.  It is assumed that matrix is 
##... a square invertible matrix.  If the inverse has never been calculated it is 
##... calculated, cached for later retrieval and returned.  If the inverse has previously
##... been calcualted the cached value is returned to the caller.
##
## Parameters:
##      x: A special "matrix" data object returned by makeCacheMatrix.
##      ...: Further arguments passed to other methods.
##
## Returns:
##      The inverse of the matrix contained in the parameter data object
##
cacheSolve <- function(x, ...) {
        ## Check if we already have the inverse matrix and return it as the result if exists
        inv <- x$getinv()
        if (!is.null(inv)){
                message("getting cached inverse matrix")
                return(inv)
        }
        
        ## If we get this far the inverse did not exist.  Calculate the inverse and cache the results.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
