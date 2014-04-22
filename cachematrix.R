## This assignment is to write a pair of functions that cache the inverse 
## of a matrix, and use the cached version if the inverse has already been 
## computed (and the matrix has not changed) rather than computing it repeatedly


## makeCacheMatrix function 
## This function creates a special "matrix" object that caches its inverse
## It returns a list of functions to:
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of the inverse of the matrix
## (4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
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
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}

