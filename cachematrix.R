# R_Coursera course
# Programming Assignment 2
# 31 Oct 2021
# Lisa M Pastore

library(readr)
print(R.version.string)
options(readr.show_col_types = FALSE)

## This function stores the inverse of a matrix in a
## separate environment for later recall. If the matrix
## inverse already exists, then the stored value is returned.
## If the inverse matrix has not be stored already, then
## this function will calculate the inverse.


makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y  ## this puts inverse of x into the cache
    m <<- NULL
    return(m)
  }
  ## I can check if the matrix is a square by...
  ##    temp<-dim(matrix)
  ##    if temp[1] ne temp[2]    
  ## then simply supply an error message.
  ## This check on the data input doesn't appear to be 
  ## expected for this assignment.
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The code below returns a matrix that is 
## the inverse of the input matrix (called from 
## cache if previously stored)

cacheSolve <- function(x, ...) {
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
