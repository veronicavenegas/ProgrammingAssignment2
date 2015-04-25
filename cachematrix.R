## Description: This file contains the functions to calculate 
#  and place in cache the inverse of a matrix.
## Developer: vvenegas
## Last Updated: Apr25, 2015
## Restrictions and assumptions: (a) the matrix supplied for evaluation is 
## a square invertible matrix.
## ***Sample code to run this functions and validate results 
## is provided at the end of this file.****


## -----------------  makeCacheMatrix -----------------
##This function creates a object that holds and serves to execute operations on the input matrix.
##The function defines the following operations: set, get, setinverse and getinverse, 
##each of which is described in this function's body

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ##  set: save in cache the matrix (x) on which the inverse will be calculated.
  set <- function(y) {
    x <<- y
    m <<- NULL ##Clear any previously cached matrix inverse results (m)
  }
  
  ##  get: optain the currently cached matrix (x)
  get <- function() x
  
  ##  set inverse: save in cache the calculated inverse (m) of 
  ##the matrix currently stored in cache (x)
  setinverse <- function(inverse) m <<- inverse
  
  ##  get inverse: Optain the calculated inverse of the matrix (m) stored in cache.
  getinverse <- function() m
  
  ##Return a list containing the defined operations
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}
## -----------------------------------------------


## -----------------  cacheSolve -----------------
##"This function calculates the inverse of the object returned by makeCacheMatrix function.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache."
cacheSolve <- function(x, ...) {  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m ## Return a matrix that is the inverse of input matrix 'x'
}
## -----------------------------------------------

## -----------------  Example -----------------
## Note: the example matrix used as test is taken from:
## http://www.purplemath.com/modules/mtrxinvr2.htm
## 
## 1. Run this code, or source this file
## 2. In the console enter the following
##    k <- matrix(c(1,0,5,2,1,6,3,4,0), 3, 3)
##    mcm <- makeCacheMatrix(k)
##    cacheSolve(mcm)
##
## The result will be:
##  [,1] [,2] [,3]
##  [1,]  -24   18    5
##  [2,]   20  -15   -4
##  [3,]   -5    4    1
##
## which is the invers matrix of the original parameter
##  > k
##  [,1] [,2] [,3]
##  [1,]    1    2    3
##  [2,]    0    1    4
##  [3,]    5    6    0
## -----------------------------------------------
