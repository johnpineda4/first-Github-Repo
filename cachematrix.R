## R Programming
## Assignment: Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  specialmatrix <- NULL
  set <- function(y) {
    x <<- y
    specialmatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) specialmatrix <<- inverse
  getinverse <- function() specialmatrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  specialmatrix <- x$getinverse()
  if(!is.null(specialmatrix)) {
    message("Cached data:")
    return(specialmatrix)
  }
  data <- x$get()
  specialmatrix <- solve(data)
  x$setinverse(specialmatrix)
  return(specialmatrix)
}
