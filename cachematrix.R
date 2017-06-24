## Put comments here that give an overall description of what your
## functions do

## The file creates two R functions makeCacheMatrix and cacheSolve. 
## The first function creates a special "matrix" object that can cache its inverse
## The second function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache

## Write a short comment describing this function
## The following function creates a special matrix object containing functions 
## to set and get the value of matrix, set and get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(InvM) m <<- InvM
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function calculates the inverse. If the inverse has already been calculated
## it returns the value from cache. Otherwise, it computes inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
