## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
##set the value of the vector
#get the value of the vector
##set the value of the inverse
##get the value of the inverse
## The method accepts an argument of class - matrix and returns the special "vector"


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

##The following function calculates the inverse of the special "vector" created
##with the above function. However, it first checks to see if the inverse has 
##already been calculated. If so, it gets the inverse from the cache and skips the 
##computation. Otherwise, it calculates the inverse of the data and sets the value 
##of the inverse in the cache via the setInverse function.

cacheSolve <- function(xx, ...) {
  ## Return a matrix that is the inverse of 'xx'
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
