## This assignment is to write a pair of functions that cache the inverse 
## of a matrix, and use the cached version if the inverse has already been 
## computed (and the matrix has not changed) rather than computing it repeatedly
## Usage: cacheSolve(makeCacheMatrix(A)), where A in an invertible matrix

## makeCacheMatrix function 
## This function creates a special "matrix" object that caches its inverse
## It returns a list of functions to:
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of the inverse of the matrix
## (4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  
  ## function to set the input matrix, and initialize cache to NULL
  ## x (input matrix) and m (computed inverse) are cached
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## function to get the input
  get <- function() x
  
  ## function to set the inverse in cache
  setinverse <- function(inverse) m <<- inverse
  
  ## funtion to return the inverse from the cache
  getinverse <- function() m
  
  ## return a list of the set/get/setinverse/getinverse functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function
## This function computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix`. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## first check the cache for inverse
  m <- x$getinverse() 
  
  ## if the inverse is in cache, return the cached value, thus avoiding recomputation
  if(!is.null(m)) { ## 
    message("getting cached data")
    return(m)
  }
  
  ## if the inverse for the input matrix is not in cache, compute it
  ## and store it in cache
  data <- x$get() ## get the input matrix
  m <- solve(data, ...)  ## compute the inverse
  x$setinverse(m)  ## store the inverse in cache
  
  ## return the inverse 
  m  
}

