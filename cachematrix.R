## These functions allow the inverse of a matrix to be cached
## so that its inverse need not be calculated

## This function creates a matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
   x<<- y
   m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve()
  getInv <- function() m
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}



## This function computes the inverse of the matrix returned by makeCacheMatrix - if already 
## calculated, it retreives the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}