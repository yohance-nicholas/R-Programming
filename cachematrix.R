
# Description -------------------------------------------------------------

## The cachematrix programming assignment was prepared in partial fulfilment of
## the requirements of the R Programming Course offered by Coursera. The purpose
## of the assignment was to demonstrate not only an understanding of how to
## cache potentially time-consuming computations but also to exhibit an
## internalisation of the principles of Lexical Scoping or Static Scoping. The
## scoping rules for R are the main feature that make it different from the
## original S language and determine how a value is associated with a free
## variable in a function

# Functions ---------------------------------------------------------------

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse. The <<- operator is utilised in this programming
## assignment to assign a value to an object in an environment that is different
## from the current one. the makeCacheMatrix function is the first of the two
## functions used to create a special object that stores a matrix and caches
## its inverse

makeCacheMatrix <- function(x = matrix()) { 
  i <- NULL
  set <- function(y) {
    x <<- y #  
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

