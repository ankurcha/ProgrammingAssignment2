# The functions in this file are inteded to produce a special "matrix" that has
# the ability to cache it's response. (if the matrix is invertible).
#
# 'makeCacheMatrix' - given a regular matrix, produces a special cache-enabled
#                     matrix version (as a list with get, set, get)
# 'cacheSolve' - computes the inverse (with caching)

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL

  get <- function() x
  set <- function(y) {
    x <<- y
    s <<- NULL
  }

  getsolve <- function() s
  setsolve <- function(solve) s <<- solve

  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


# This function computes the inverse of the special "matrix" returned by
# `makeCacheMatrix` above. If the inverse has already been calculated (and
# the matrix has not changed), then `cacheSolve` should retrieve the inverse
# from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
