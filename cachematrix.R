## These two functions allow for the inverse of a matrix to be calculated once
## and cached, then read back from the cache whenever needed, so as to avoid
## costly recalculations.

## makeCacheMatrix creates a special "matrix" that is actually a list of functions.
## The purpose is to store the matrix in the global variable x, calculate its
## inverse and store it in global variable i, and read i back when necessary.
## "set" stores the matrix in global variable x and initializes the inverse.
## "get" reads the matrix from the global variable.
## "setInverse" sets the inverse that is returned by cacheSolve.
## "getInverse" gets the inverse from the global variable i.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve gets the inverse from the "matrix" created by makeCacheMatrix.
## If the inverse has already been calculated, it reads it from the cache.
## If not, it calculates it for the first time and caches it using setInverse.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
