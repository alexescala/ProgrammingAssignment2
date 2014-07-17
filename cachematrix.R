## This file contains two functions. The purpose of the functions
## is (1) to create a special matrix object which can cache its inverse
## and (2) to provide a special "invert" method which first tries to
## retrieve the cached inverse and if this is not found it computes it and saves it.

## Creates a special matrix, which is really a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of its inverse
## get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }



## When given a special matrix returned by the function makeCacheMatrix,
## it first checks to see if the inverse of the matrix has already been computed.
## If so, it retrieves the inverse. Otherwise, it computes the inverse and caches 
## it in the special matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
