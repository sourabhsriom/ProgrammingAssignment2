## These functions are designed to enable a user to cache the inverse 
## of a matrix to reduce the computational costs. Once the inverse of
## a matrix is computed, it can be re-used if needed.

## This is a function to store the inverse of matrix after the first
## computation. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function computes the inverse of a matrix the first time and 
## and sets its value for a matrix, the next time if the same matrix 
## is called, the cached value is retreived.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
