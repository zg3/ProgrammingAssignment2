## This program conatins a pair of functions that compute and cache the inverse of a matrix.

## Function makeCacheMatrix creates a list of four functions to do the following:
## 1. set the value of the matrix
## 2. get the value of the matrix 
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setmatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(setmatrix = setmatrix, 
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cacheSolve returns the inverse of the matrix returned by makeCacheMatrix above.
## First it checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse and sets the value in the cache via the setinverse function.

# This function assumes that the matrix is always invertible, and calls the solve function to compute the inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$getmatrix()
  i <- solve(data)
  x$setinverse(i)
  i  
}

## Sample run:
## > m <- matrix(1:4, 2,2)
## > v <- makeCacheMatrix(m)
## > v$getmatrix()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## first call - no inverse data in chache
## > v$getinverse()
## NULL
## > cacheSolve(v)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## second call - data retrieved from cache
## > cacheSolve(v)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##