# Assignment2: Caching the Inverse of a Matrix

## creates a wrapper object around a matrix that can optionally store
## its inverse. exposes setters and getters for both. invalidates
## cache on set.
makeCacheMatrix <- function(x = matrix()) {
  # initialize the value 
  tmp <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    tmp <<- NULL
  }
  #  get the value of the matrix
  get <- function() x
  # set the inverse
  setinv <- function(inv) tmp <<- inv
  # get the inverse
  getinv <- function() inverse
  # return a list of all the above functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
 

## Computes the inverse of a matrix with caching which returned by a function
## makeCacheMatrix.
##    = checks cache in the object passed in for the presence of the result
## of a previous run. if found returns that, otherwise computes the result and 
## stores it in the cache and returning it to the caller

cacheSolve <- function(x) {
  # check whether of not the inverse is cached
  tmp <- x$getinv()
  if(!is.null(tmp)) {
    message(" current cached data")
    return(tmp)
  }
  # not cached, get the matrix into data
  data <- x$get()
  tmp <- solve(data)
  x$setinv(tmp)
  result <- tmp
  result
