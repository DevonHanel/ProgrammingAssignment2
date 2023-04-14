# A function that creates a matrix capable of caching its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# A function that can compute the inverse of the  matrix created by makeCacheMatrix()
cacheSolve <- function(x, ...) {
  # Returns the cached result if available
  i <- x$getInverse()
  if (!is.null(i)) {
    message("Retrieving cached inverse")
    return(i)
  }
  
  # This computes the inverse if it is not cached
  mat <- x$get()
  i <- solve(mat, ...)
  
  # Caches the inverse
  x$setInverse(i)
  
  # This one here returns the inverse
  i
}