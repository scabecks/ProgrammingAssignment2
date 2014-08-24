## These functions: 1) create a 'special' matrix object which can can cache 
## its inverse, and; 2) computes the inverse of the object returned above
## , or if that object has already been cached, retreives the object from
## the cache.

## Function to create a 'special' matrix object, which can cache its
## inverse.

makeCacheMatrix <- function (x = matrix()) {
  m <- matrix()
  set <- function(y = matrix()){
    x <<- y
    m <<- matrix()
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function to retrieve the cached inverse of the object produced
## by the above function,or if not found, calculates the inverse of
## the object

cacheSolve <- function(x, ...){
  m <- x$getinv()
  if(!is.na(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
}
