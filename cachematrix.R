## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## This program assumes that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  inverse1 <- NULL
  set <- function(y)   # Set the value of the matrix
  {
    x <<- y
    inverse1 <<- NULL
  }
  get <- function(y) x  # Get the value of the matrix
  setinverse <- function(inverse) inverse1 <<- inverse   #Set the value of the inverse of the matrix
  getinverse <- function() inverse1                      #Get the value of the inverse of the matrix
  list(set=set, get=get,setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
## changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse1 <- x$getinverse()
  if(!is.null(inverse1)){
    message("Getting the Cached Data")
    return(inverse1)
  }
  data <- x$get()
  inverse1 <- solve(data)
  x$setinverse(inverse1)
}
