## The code in this file, cachematrix.R, consists of two functions that are 
## meant to be used together to cache the inverse of a matrix.
##    1.The first function is called makeCacheMatrix 
##    2.The second function is called cacheSolve 
## Caching the inverse of a matrix can speed up access to the inverse when 
## the inverse matrix is requested repeatedly.  This is particularly helpful 
## when using large sized matrices.


## The makeCacheMatrix creates a special "matrix", which is really a list 
## containing functions to:
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse of the matrix
##    4. get the value of the inverse of the matrix

makeCacheMatrix <-function(x = matrix()) {
    
  inverse = NULL #set inverse of matrix to NULL
    
    set = function(y) {
      # the use of <<- assigns a value to an object in an environment
      # different from the current environment.
        x <<- y 
        inverse <<- NULL
    }
    
    get <- function() x # Get the value of the matrix
    setinverse <- function(inv) { # Sets inverse of the matrix
        inverse <<- inv  # Set inverse matrix in environment different 
                         # from current environment
    }
    getinverse <- function() inverse # Get inverse of matrix
    list(set=set,
         get=get, 
         setinverse=setinverse, 
         getinverse=getinverse
         ) # return a list containing the four functions
}


## The cacheSolve function returns the inverse of the original matrix that was
## input into the makeCacheMatrix function. However, it first checks to see if
## the inverse matrix has already been calculated. If so, it gets the inverse
## matrix from the cache and skips the computation. Otherwise, it calculates 
##  the inverse matrix and sets the value of the inverse matrix in the cache.

cacheSolve <- function(x, ...) {
  
    inverse <- x$getinverse() # Get inverse matrix from "special matrix" created
                              # by the makeCacheMatrix function

    # If statement says to check if the inverse has already been calculated
    # If it has, then skip computation and show message of 
    # "getting cached data" and return the inverse matrix.  
    if (!is.null(inverse)){
        message("getting cached data")
        return(inverse) #return the inverse matrix
    }
    
    # Otherwise, if inverse matrix does not exist it says to get the matrix
    # calculate the inverse, set the inverse matrix, and return inverse matrix.
    data <- x$get() # get the matrix
    inverse <- solve(data, ...) # compute the inverse of the matrix
    x$setinverse(inverse) # set the value of the inverse matrix in the cache
    inverse # return the inverse matrix
}

