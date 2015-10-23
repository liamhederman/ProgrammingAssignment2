## There are two functions below: makeCacheMatrix and cacheSolve
## makeCacheMatrix is matrix that caches its inverse
## cacheSolve checks if the inverse has been calculated and then returns the inverse

## The cacheMatrix function creates a pseudo class of a matrix, that is able to cache its own inverse
## It contains four functions within it: a getter, a setter, a get invers and a set inverse. 
makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
  set <- function(y) {
    x <<- y
    matrix_inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) matrix_inv <<- inv
  getInverse <- function() matrix_inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve gets the inverse from x, checks if its uncalculated, and if it isn't solves
## and sets the object
cacheSolve <- function(x, ...) {
  m <- x$getInv() 
  if(!is.null(m)) { 
    message("getting cached data")
    return(m) 
  }
  data <- x$get() 
  m <- solve(data) 
  x$setInv(m)
}
