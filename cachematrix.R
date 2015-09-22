## This pair of functions caches the inverse of a matrix
## Functions assume that input matrix is always invertible

## makeCacheMatrix -  creates a 'matrix' object that can cache its inverse
## input - x, a matrix
makeCacheMatrix <- function (x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve - computes the inverse of a 'matrix' object returned by makeCacheMatrix
## input -  'matrix' returned by makeCacheMatrix
## output - inverse of input 'matrix'
## if inverse has already been calculated for a matrix, retrieves inverse from cache
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
