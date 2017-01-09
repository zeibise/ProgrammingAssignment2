## This R script contains two functions.
## Function  makeCacheMatrix makes a special matrix that has the capability
## to cache its inverse. 
## The function cacheSolve is the function for computing this inverse.

## makes a special matrix that has the capability to cache its inverse.
## the input must be an invertible matrix
## use: Let A be an invertible matrix
## the inverse can be cached by calling
## a <-makeCacheMatrix(A)
## cacheSolve(a)

## Now A can be retrived by a$get() and its inverse by cacheSolve(a).
## The cacheSolve(a) reads the inverse from cache after the first call. Matrix A can
## be edited to A' with a$set(A'), and this empties the cache accordingly. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <-function(inverse) m <<-inverse
  getInverse <-function() m
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This solves the inverse of matrix x that has been produced by the makeCacheMatrix.
## The inverse is read from cache if available.

cacheSolve <- function(x, ...) {
    m <-x$getInverse()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    data <-x$get()
    m <-solve(data)
    x$setInverse(m)
    return(m)
}
