## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  xinv <- matrix()
  set <- function(y = matrix()) {
    x <<- y
    xinv <<- matrix()
  }
  get <- function() x
  setinv <- function(inverse) xinv <<- inverse
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  xinv <- x$getinv()
  if(!sum(is.na(xinv))) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinv(xinv)
  xinv
}
