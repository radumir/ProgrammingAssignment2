## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function provides a mean to store in cache the inverse of a matrix
## in order to save computation time.
## The x argument is a matrix and in it's space will be injected 4 functions get, set, 
## setInverse( inverseMatrix ) and getInverse(): matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}


## Solve the matrix by searching in the cache for the inverse, and, if it is found,
## the cached inverse matrix will be returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
