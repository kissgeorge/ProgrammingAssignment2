# Functions for creating and using inverted matrices which caching ability
# Creates cacheable matrix for inputting to
# cacheSolve() function which sets and gets the cached values

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of the inverse of the matrix
  setsolve <- function(solve) m <<- solve
  # get the value of the inverse of the matrix
  getsolve <- function() m
  # return our list
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


# Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
# If the inverse has already been done and there's no change in the matrix
# then the cacheSolve() returns the cached inverse

cacheSolve <- function(x, ...) {
  inverted.matrix <- cacheable.matrix$get.inverse()
  # check cached matrix
  if(!is.null(inverted.matrix)) {
    message("Getting cached inverse matrix")
    return(inverted.matrix)
  }
  # Let's create inverted matrix in case
  # there's no cached matrix available.
  matrix.to.inverse <- cacheable.matrix$get()
  inverted.matrix <- solve(matrix.to.inverse)
  cacheable.matrix$set.inverse(inverted.matrix)
  inverted.matrix
}
