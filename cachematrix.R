## This project contains two functions which can be used to
## cache the inverse of a matrix (Suppose it's invertible)


## This function will create a special "matrix" object that 
## can cache its inverse. Indeed the returned value would
## be a list of for functions.

makeCacheMatrix <- function(x = matrix()) {

  xinv <- matrix()
  ## Set the matrix  
  set <- function(y = matrix()) {
    x <<- y
    xinv <<- matrix()
  }
  ## Get the matrix
  get <- function() x
  ## Set the inverse
  setinv <- function(inverse) xinv <<- inverse
  ## Get the inverse
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function returns the inverse of the special "matrix"
## object returned by the makeCacheMatrix() function above.
## If the inverse has already been calculated (and the matrix)
## has not been changed), then this function will retrieve 
## the inverse from the cache and save the computation.

cacheSolve <- function(x, ...) {
  xinv <- x$getinv()
  
  ## if the inverse is already computed, then just retrieve
  ## it from the cache
  if(!sum(is.na(xinv))) {
    message("getting cached data")
    return(xinv)
  }

  ## get the created matrix and calculate the inverse
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinv(xinv)
  xinv
}
