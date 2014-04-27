## makeCacheMatrix function creates a special "matrix" then   
## cacheSolve calculates the inverse of the matrix.

## If the matrix inverse has been already calculated, it will 
## find the value from the cache and return.

makeCacheMatrix <- function(x = matrix()) {
  inverse.x <- NULL
  set <- function(y) {
    x <<- y
    inverse.x <- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inverse.x <<- inverse
  getinverse <- function() inverse.x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  inverse.x <- x$getinverse()
  if (!is.null(inverse.x)) {
    message("Getting cached inverse matrix")
    return(inverse.x)
  } else {
    inverse.x <- solve(x$get())
    x$setinverse(inverse.x)
    return(inverse.x)
  }
}
