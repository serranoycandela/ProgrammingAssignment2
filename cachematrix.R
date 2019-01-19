## Below are two functions that are used to create a special object that stores 
## a matrix and cache's it´s inverse matrix.


## This function creates a special "matrix" object that can cache its inverse.
## The function returns a list of functions:
## set() sets the matrix
## get() returns the matrix
## get_inverse() returns the inverse of the matrix
## set_inverse() sets the inverse of the matrix and caches it

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # Sets the matrix 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Returns the matrix 
  get <- function() x
  
  # Manually sets the inverse matrix
  set_inverse <- function(inverse) inv <<- inverse
  
  # Returns the inverse
  get_inverse <- function() inv
  
  # Return the list of functions
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## retrieves the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  inv
}
