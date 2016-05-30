## Programming Assignment 2
## This function returns the inverse of a matrix with time saving   
## caching capabilities for time consuming operation of matrix inversion

## makeCacheMatrix function creates a special vector, which is a list 
## containing functions to set, get the matrix, set, get the inverse 
## of the matrix
## Inputs:
##	x - matrix if provided, otherwise x is initialized to empty matrix
## Returns:
##	A list of functions
makeCacheMatrix <- function(x = matrix()) {
  ## initialize inverse of the matrix
  inv <- NULL
  ## assign matrix x to be matrix y, reset inv of matrix x to NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  ## return matrix x
  get <- function() x
  ## set inverse of matrix x
  setInverse <- function (Inverse) {inv <<- Inverse}
  getInverse <- function() inv
  ## function returns a list
  list(set = set, get = get, setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve function computes and returns the inverse of the matrix 
## provided with time saving technique of cacheing, so only one 
## computation is needed with multiple reads
## Inputs: 
##	x - matrix
##      ... 
## Returns:
##	invers of matrix x   
cacheSolve <- function(x, ...) {
  ## read Inv saved with object (matrix) x
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    # Data exists already,return right away
    message("getting cached data")
    return(inv)
  }
  ## get matrix x
  data <- x$get()
  ## compute the inverse
  inv <- solve(data, ...)
  ## cach the inverse
  x$setInverse(inv)
  ## return the inverse
  inv
}
