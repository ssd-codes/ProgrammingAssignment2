## To calculate inverse of a matrix
## makeCacheMatrix creates a new matrix with input data
## it has the set and get functions to initialize original matrix 
## and also the inverse
## Rather than calculating the inverse each time, this function returns
## the cached inverse if the original matrix has not changed

## makeCacheMatrix creates a new matrix with input data
## it has the set and get functions to initialize original matrix 
## and also the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL 
  
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x 
  
  setInverse <- function(inv) inverse <<- inv
  
  getInverse <- function() inverse
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  
}


## First time it computes the inverse and puts it in the cache
## Rather than calculating the inverse each time, this function returns
## the cached inverse if the original matrix has not changed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Returning inverse from cache")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setInverse(inverse)
  inverse
}
