## R Programming Assignment 2 for coursera: Data Science 
##  created: 09/05/2020     Created by: Ana Morse Buckland
##  assignment is to write a pair of functions that cache the inverse of a matrix.

## This pair of R functions is able to cache the inverse of a square matrix only. 
## If you want to use  the is.square.matrix() of R install the package and library(matrixcalc)
##    Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
##    of a matrix rather than compute it repeatedly.

## The function makecacheMatrix creates 4 functions that
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse of the matrix
##    get the value of the inverse of the matrix
## It returns a special vector which is really a list containing these 4 functions
## Example of how to run:  
##    amatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
##    cacheSolve(amatrix)

## This function creates a special "matrix" object that can cache its inverse.
#library(matrixcalc)
makeCacheMatrix <- function(x = matrix()) {
  # initialize minv to Null; this will hold the inverse of a matrix
  minv <- NULL
  # Function to set the value of the matrix
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  # Function to get the value of the matrix
  get <- function() x
  # function to set the value to the inverse of a matrix
  setminvrs <- function(minvrs) minv <<- minvrs
  # function to get the value of the inverse of a matrix
  getminvrs <- function() minv
  # a special list is Returned containing the 4 functions above
  list(set = set, get = get,
       setminvrs = setminvrs,
       getminvrs = getminvrs) 
}


## Function: cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##  retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minv <- x$getminvrs()
  if(!is.null(minv)) {
    message("getting cached data of the matrix inverse")
    return(minv)
  }
  data <- x$get()
  if (is.square.matrix(data) == TRUE) {
    minv <- solve(data, ...)
    # sets the value of the inverse in the cache
    x$setminvrs(minv)
    minv}
  else
  { message("this is not a square matrix: try again")}  
  
}
