## Matrix inversion is usually a costly computation and their may be 
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly (there are also alternatives to matrix inversion that 
## we will not discuss here). Your assignment is to write a pair of
## functions that cache the inverse of a matrix.

## Computing the inverse of a square matrix can be done with the solve 
## function in R. For example, if X is a square invertible matrix, then
## solve(X) returns its inverse.


makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its
  ## inverse.
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special "matrix" returned
  ## by makeCacheMatrix above. If the inverse has already been calculated
  ## (and the matrix has not changed), then the cachesolve should retrieve 
  ## the inverse from the cache.
  m <- x$getinv()                 #query the x matrix's cache         
  if(!is.null(m)) {               #if there is a cache
    message("getting cached data") 
    return(m)                     #just return the cache, no computation needed
  }
  data <- x$get()                 #if there's no cache
  m <- solve(data, ...)           #we actually compute them here
  x$setinv(m)                     #save the result back to x's cache
  m                               #return the result
}
