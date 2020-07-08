##These functions compute and cache the inverse of a matrix.

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) {
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invert <<- inverse
  getInverse <- function() invert
  list(set = set, get = get, setInverse = setInverse, getInverse = 
         getInverse)
}

##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
  invert <- x$getInverse()
  if(!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  newInvert <- x$get()
  invert <- solve(newInvert, ...)
  x$setInverse(invert)
  invert
}