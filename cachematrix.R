## Below are my two functions, one that can cache a matrix and its inverse then second to retrieve the cached value. I used the 'makevector()' and 'cachemean()' functions as the base, substituting in the solve() function to find the inverse of a matrix in place of the mean() function.

## This function creates a special "matrix" object that can cache its inverse. This can then be retreived using the cacheSolve function detailed below.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
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


## This function computes the inverse of a special "matrix" returned by 'makeCacheMatrix'. If the inverse has already been calculated (matrix not changed) cacheSolve should retrieve the inverse of the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
