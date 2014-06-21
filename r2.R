## The following functions can be used to cache the inverse of a matrix.

## The first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  } ## create a matrix x
  get <- function() x ## retrieve the matrix x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve) ## a list of the four functions     
}

## The second function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } ## If the inverse has already been calculated (and the matrix has not changed), 
  ## then the cacheSolve retrieves the inverse from the cache.
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  ## Return a matrix that is the inverse of 'x'
}