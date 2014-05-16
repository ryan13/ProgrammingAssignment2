# Assignment2: Caching the Inverse of a Matrix

## creates a wrapper object around a matrix that can optionally store
## its inverse. exposes setters and getters for both. invalidates
## cache on set.
makeCacheMatrix <- function(x = matrix()) {
  tmp <- NULL
  set <- function(y) {
    x <<- y
    tmp <<- NULL
  }
  get <- function() x
  setinv <- function(inv) tmp <<- inv
  getinv <- function() inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## computes the inverse of a matrix with caching.
## expects a wrapped matrix as returned from makeCacheMatrix. first
## checks cache in the object passed in for the presence of the result
## of a previous run. if found returns that, otherwise computes the
## result and then stores it in the cache as well as returning it to
## the caller

cacheSolve <- function(x) {
  tmp <- x$getinv()
  if(!is.null(tmp)) {
    message(" current cached data")
    return(tmp)
  }
  data <- x$get()
  tmp <- solve(data)
  x$setinv(tmp)
  result <- tmp
  result
