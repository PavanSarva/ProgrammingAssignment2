## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(xx = matrix()) {
  i <- NULL
  set <- function(yy) {
    xx <<- yy
    i <<- NULL
  }
  get <- function() xx
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(xx, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- xx$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- xx$get()
  i <- solve(data, ...)
  xx$setInverse(i)
  i
}
