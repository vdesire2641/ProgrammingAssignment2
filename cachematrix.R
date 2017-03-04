## I am caching the inverse of a matrix
## caching the matrix is beneficial than repeatedly 
##computing it.

## The function below creats a matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
  

## the function below computes the inverse of the 
## matrix above. It should be able to retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting the data cached")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr, ...)
  x$setInverse(inv)
  inv
}
