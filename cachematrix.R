# Kevin Watt, rprog-010.  1/24/2015

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # m = our matrix, NULL by default
  m <- NULL
  
  # set it - this will clear the cached solution
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get it
  get <- function() x
  
  # save the matrix inverse ("solution")
  setsolve <- function(solve) m <<- solve
  
  # get the saved result
  getsolve <- function() m
  
  # let us access these functions from the return value list "object" 
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  # check for cached solution
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # get data from our "CacheMatrix" object
  data <- x$get()
  
  # preform the expensive inverse computation ("solve" = inverse)
  m <- solve(data, ...)
  
  # save it to our object
  x$setsolve(m)
  
  # return the solution
  m  
}
