## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) Inv <<- inverse
  getInverse <- function() Inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" created above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInverse()
  if(!is.null(Inv)) {
    message("Getting cached data")
    return(Inv)
  }
  matrix <- x$get()
  Inv <- solve(matrix, ...)
  x$setInverse(Inv)
  Inv
}
