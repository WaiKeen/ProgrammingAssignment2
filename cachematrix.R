## Programming Assignment 2 for Coursera R Programming
## MakeCacheMatrix funtion creates a special "matrix" object that can cache its inverse
## contains a list of functions
## 1. set = set the Value of the matrix
## 2. get = get the value of the matrix
## 3. setinv = set the inverse value of the matrix
## 4. getinv = get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve calculate the mean of the special "matrix" created by MakeCacheMatrix function
## will initially check if the inverse has already been calculated
## If inverse has been calculated, it will return the cache and skips that calculation
## Otherwise, it calculates the inverse of the data and sets the value in the cache via setinv function
## It will return a matrix that is the inverse of x

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  
  if (!is.null(i)) {
    message("getting cache data")
    return (i)
  }
  
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  i
}
