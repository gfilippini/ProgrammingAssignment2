## This program calculates the inverse of a square matrix
## Since calculation of an inverse of a matrix could be costly on computation 
## resources, the program store the calculated inverse on cache. 
## If needed and the inverse is already calculated, it recovers the inverse from cache
## It's assumed that the input matrix has an inverse (it's not singular)

## The function makeCacheMatrix stores a matrix on cache

makeCacheMatrix <- function(x = matrix()) {
  inv  <- NULL
  set  <- function(y){
    x   <<- y
    inv <<- NULL 
  }
  get  <- function() x
  setinv  <- function(inverse) inv  <<- inverse
  getinv  <- function() inv
  list(set= set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## The function cacheSolve computes (and stores) the inverse of the matrix cached 
## on the fuction makeCacheMatrix
## if the inverse is already calculated it gets the cached data

cacheSolve <- function(x, ...) {
  ## Get inverse of matrix 'x' that is cached
  inv  <- x$getinv()
  ## if it exists display the inverse already calculated
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## if don't exists calculate the inverse (by solve) and stores it on cache
  data  <- x$get()        
  inv  <- solve(data, ...)
  x$setinv(inv)
  inv
}
