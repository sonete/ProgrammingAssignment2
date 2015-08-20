## makeCacheMatrix and cacheSolve use scoping rules to
## cache inverse of matrix.

## makeCacheMatrix creates object that is a matrix that has inverse cache-able

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { # setter function
    x <<- y
    m <<- NULL
  }
  get <- function() { x } #getter function
  setinv <- function(matinv) { inv <<- matinv }
  getinv <- function() { inv }
  list(set = set, get = get, setinv = setinv, getinv = getinv) # return the list of functions
}


## cacheSolve calculates the inverse of a matrix only the first time.
## Every subsequent call it retrieves the value from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){ # if not null then we have it in cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv #return value
}
