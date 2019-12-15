## The functions attempt to cache the inverse of a matrix

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse
## The special "matrix" is a list containing a function to set and get the values of the matrix, and set and get the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting inverse matrix")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setinv(m)
  m
}