## This function calculates the inverse of a matrix for the first time and stores it a different environment (cache)
## When the user wants the inverse for the same matrix for the sebsequent time, the stored cache data is returned


## Creates a special "vector", which is a list containing the following functions
## a) Set the value of matrix
## b) Gets the value of the matrix
## c) Sets the inverse of the matrix
## d) Gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(z) inverse <<- z
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function looks for any stored cache data (containing inverse of the matrix) and returns it;
## Else it calculates the cache and stores it so that it can be used next time

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
