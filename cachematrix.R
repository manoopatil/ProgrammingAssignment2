## Below are two functions that are used to create a 
## special object that stores a numeric matrix and cache's its inverse.


## This first function makeCacheMatrix creates a special "matrix" and cache's its inverse, 
## which is really a list of functions to
## 1. set the value of the Matrix
## 2. get the value of the Matrix
## 3. set the value of the inverse of Matrix
## 4. get the value of the inverse of Matrix

makeCacheMatrix <- function(x = matrix()) {

  inx = matrix() 
  inx <- NULL

  ## set the value of Matrix and initialize inverse of it
  set <- function(y){
    x <<- y
    inx <<- NULL
  }
  
  ## get the value of Matrix
  get <- function() x
  
  ## set the value of inverse of matrix, it does not perform the calculation though, but faciliate
  ## the caching via "<<"
  setinvm <-function(solve) inx <<- solve
  
  ## get the values for inverse of matrix
  getinvm <- function() inx
  
  ## creates the list of functions which can be used by the next fuction
  list(set = set, get = get,
       setinvm = setinvm,
       getinvm = getinvm)

}


## following function calculates theinverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setinvm function.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  ## Get the value for inverse
  inx <- x$getinvm()
  
  ## If already exists in cache, return from the cache
  if (!is.null(inx)){
    message ("getting cached data")
    return(inx)
  }
  
  ## Get the value of input "matrix" in data 
  data <- x$get()
  
  ## calculates the inverse of the "matrix" using solve function
  inx <- solve(data,...)
  
  ## set value for the inverse
  x$setinvm(inx)
  
  ## returns the value for inverse
  inx
}
