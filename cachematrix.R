## Cached matrix system that saves computing power
## when finding the inverse

## Will establish a cached matrix storing the info
## about it's inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## The cache solve will use the solve function to inverse a 
## cached matrix. If it has already been inversed it will pull
## from the cache saving computing.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("Retrieving cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
