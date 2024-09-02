# Assignment: Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and there may be 
# some benefit to caching the inverse of a matrix rather than computing 
# it repeatedly (there are also alternatives to matrix inversion that we 
# will not discuss here). Your assignment is to write a pair of functions 
# that cache the inverse of a matrix.

# Write the following functions:
  
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
# should retrieve the inverse from the cache.

# Computing the inverse of a square matrix can be done with the solve function in R. For example, 
# if X is a square invertible matrix, then solve(X) returns its inverse.

# For this assignment, assume that the matrix supplied is always invertible.


makeCacheMatrix <- function(x = matrix()) {
  
  # store the cached inverse as z, set as NULL  
    z <- NULL
    
  # set up the matrix
    set <- function(y) {
      
  # <<- operator assigns a value to an object in an environment that is different from the current environment.
    x <<- y
    z <<- NULL
    }
  
  # Get the matrix value  
    get <- function() x 
    
    setinverse <- function(inverse) z <<- inverse
    getinverse <- function() z
    
    list(set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  z <- x$getinverse()
  
  # If inverse is already been cached, return it
  if(!is.null(z)){
    message ("getting cached data")
    return(z)
  }
  
  # If inverse is not cached, then need to solve for it
  data <- x$get()
  z <- solve(data, ...)
  
  x$setinverse(z)
  
  # Return the inverse
  z
}

