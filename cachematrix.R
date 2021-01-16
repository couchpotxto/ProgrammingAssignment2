##Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
##Caching the Inverse of a Matrix
  ##pair of functions are written to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  #makeCacheMatrix: this function creates a special "matrix" object that can cache its inverse.
  a <- NULL
  set <- function(y){
    x <<- y
    a <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) a <<- inverse
  getInverse <- function() a 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  ##cacheSolve: this function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
    ##If the inverse has already been calculated (and the matrix has not changed), 
    ##then the cachesolve should retrieve the inverse from the cache.
  a <- x$getInverse()
  if(!is.null(a)){
    message("obtaining cached data")
    return(a)
}
  mat <- x$get()
  a <- solve(mat,...)
  x$setInverse(a)
  a
}

