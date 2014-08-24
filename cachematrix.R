## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix(x) : given a matrix x the function returns a "matrix object"
## cacheSolve(x, ...) : given a "matrix object" the function returns the (possibly cached) inverse of the matrix

## Write a short comment describing this function
##
## makeCacheMatrix(x) creates a "matrix object" given a matrix x.
## the "matrix object" is a list containing functions to set and get the matrix,
## as well as to set (setInv) and get (getInv) the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inv) i <<- inv
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
##
## cacheSolve(x, ...) returns the inverse of the matrix contained on the "matrix object" x.
## If the inverse has been previously calculated, and the matrix has not been modified,
## the function returns the cached inverse.
## Otherwise, cacheSolve calculates, saves and returns the matrix inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setInv(inv)
  return(inv)
}