## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  inv <- NULL
  ## Set the matrix
  set <- function( matrix ) {
    x <<- matrix
    inv <<- NULL
  }
  
  ## Get the matrix
  get <- function() {
    ## Return the matrix
    x
  }
  
  ## Set the inverse of matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Get the inverse of matrix
  getInverse <- function() {
    ## Return the inverse 
    inv
  }
  
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return the inverse of matrix "x"
  inv <- x$getInverse()
  
  ## Return the inverse if its has already cached
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Get the matrix from the object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  inv <- solve(data, ...)
  
  ## Set the inverse of the object
  x$setInverse(inv)
  
  ## Return the inverse
  inv
}
