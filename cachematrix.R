## makeCacheMatrix: function creates a special matrix object that can cache its inverse.

## This program assumes that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) 
{
  invrs <- NULL
  set <- function(y)   # Set the value of the matrix
    {
    x <<- y
    invrs <<- NULL
    }
  get <- function(y) x  # Get the value of the matrix
  setinverse <- function(inverse) invrs <<- inverse   #Set the value of the inverse of the matrix
  getinverse <- function() invrs                      #Get the value of the inverse of the matrix
  list(set=set, get=get,setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: function computes the inverse of the special matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
## changed), then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  invrs <- x$getinverse()
  if(!is.null(invrs))
    {
    message("Getting the Cached Data")
    return(invrs)
    }
  data <- x$get()
  invrs <- solve(data)
  x$setinverse(invrs)
}
