## These functions should set and get a Matrix, calculate it's inverse, cache it and get it.

## makeCacheMatrix() is a function listing 4 sub-functions :
## 1.set : sets a matrix
## 2.get : gets it
## 3.setinv : set its inverse and cache it
## 4.getinv : gets the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmat <- function(y){
    x <<- y
    inv <- NULL
  }
  getmat <- function() x 
  setinv <- function(inverse) inv <<- inverse  
  getinv <-function() inv
  list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
  
  
}


## cacheSolve() gets the inverse of a matrix. It verfies if the inverse is already cached. 
##If it is it will get the cached data, else it computes and prints the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting Cached Invertible Matrix")
    return(inv)
  }
  data <- x$getmat()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}