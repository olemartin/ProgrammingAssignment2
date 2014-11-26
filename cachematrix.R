## A couple of functions that can cache and create an inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  inversedMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    inversedMatrix <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse) inversedMatrix <<- inverse
  getInverse <- function() inversedMatrix
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function creates an inverse of the matrix x. Unless the inverse is 
## already created, then it returns the cached inversed matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inverse = x$getInverse()
       if (is.null(inverse)) {
         inverse = solve(x$get())
         x$setInverse(inverse)
       }
       inverse
}
