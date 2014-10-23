## The two functions that follow (MakeCacheMatrix and cacheSolve)
## cache the inverse of a matrix.
## In this way, we avoid to recalculate the inverse of a matrix that was previously
## calculated and have not changed.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It return value is a list with 4 elements (functions):
## first: set the matrix value
## second: get the matrix value
## third: set the inverse matrix
## fourth: get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function
## calculates the inverse of the special "matrix" created with the function makeCacheMatrix
## Before calculating the inverse matrix, it checks if it was already calculated.
## If this is the case, it returns the message "getting cached data" 
## and the previously calculated value of the inverse matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
