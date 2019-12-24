## Overall I am setting up two generic functions that act in conjunction with one another.  The purpose of
## these functions is to be able to quickly and repeatedly find the inverse of any invertible matrix.

## This first function is a generic function that takes a matrix and returns a list of four functions.
## The four functions are essentially empty, but are filled when you type in arguments to it and to 
## cacheSolve below.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get<-function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This second function is meant to find the actual inverse matrix.  It first checks to see if the inverse
## of matrix x (or whatever your matrix may be) has already been calculated (in other words, isn't NULL).
## If this is so, then it uses the cache to return the inverse.  This saves time in the case where you'd want
## to repeatedly calculate an inverse, especially if the inverse itself takes a long time to calculate.  If
## the matrix is NULL (in other words, the first time you use cacheSolve), it calculates the inverse the long
## way.  It ultimately does this by using the "solve" command from the "data" that is assigned to the original
## matrix x.  Then, importantly, it sets the inverse up in the makeCacheMatrix function so that the next time 
## cacheSolve calls on makeCacheMatrix, the "inv" is no longer NULL, but instead is an (inverse) matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
