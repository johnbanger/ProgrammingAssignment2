## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix that is a actually a list
## containing a function that (1) sets the values of the matrix;
## (2) gets the values of the matrix; (3) sets the values of the inverse;
## (4) gets the values of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
## This function solves the inverse of the matrix in the function
## makeCacheMatrix. It first checks to see if the inverse has already
## been calculated and stored in the cache. If not, it will complete
## the inverse calculation.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
