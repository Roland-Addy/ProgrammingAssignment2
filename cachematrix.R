## These functions caches the inverse of a matrix (assuming is invertible)
## This enables faster computations of the inverse of a matrix

## This function returns a list of functions that:
##Sets a matrix
##Gets the defined matrix
##Sets the inverse of the defined matrix
##Gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y=matrix()){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function returns the inverse of the matrix defined in makeCacheMatrix if already stored in cache
##Else it calculates the inverse,stores it in cache and returns it

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
