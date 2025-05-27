## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix  
## 3. set the value of the inverse
## 4. get the value of the inverse
## Args:
##   x: a square invertible matrix (default is empty matrix)
##
## Returns:
##   A list containing the four functions described above

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cached inverse to NULL
  inv <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y          # Set the matrix in the parent environment
    inv <<- NULL     # Reset the cached inverse since matrix changed
  }
  
  # Function to get the matrix
  get <- function() {
    x
  }
  
  # Function to set the cached inverse
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Function to get the cached inverse
  getInverse <- function() {
    inv
  }
  
  # Return a list of functions
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
    ##Computes the inverse of the special "matrix" created by makeCacheMatrix
    ##
    ## Args:
    ##   x: a special "matrix" object created by makeCacheMatrix
    ##   ...: additional arguments to be passed to the solve function
    ##
    ## Returns:
    ##   The inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Try to get the cached inverse
  inv <- x$getInverse()
  
  # If inverse is cached (not NULL), return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not cached, compute the inverse
  data <- x$get()           # Get the matrix
  inv <- solve(data, ...)   # Compute the inverse using solve()
  x$setInverse(inv)         # Cache the computed inverse
  
  # Return the inverse
  inv
}
